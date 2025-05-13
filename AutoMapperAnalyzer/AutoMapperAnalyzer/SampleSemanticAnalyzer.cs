using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace AutoMapperAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class AutoMapperAnalyzer : DiagnosticAnalyzer
    {
        private class MappingInfo
        {
            public HashSet<string> MappedProperties { get; } = new(StringComparer.OrdinalIgnoreCase);
            public HashSet<string> IgnoredProperties { get; } = new(StringComparer.OrdinalIgnoreCase);
        }

        private List<(ITypeSymbol Source, ITypeSymbol Destination)> allMappings = [];

        public const string UnmappedPropertyDiagnosticId = "AM0001";
        public const string TypeMismatchDiagnosticId = "AM0002";
        public const string MissingMappingDiagnosticId = "AM0003"; // Новый код: для несопоставленных свойств

        private static readonly DiagnosticDescriptor UnmappedPropertyRule = new(
            UnmappedPropertyDiagnosticId,
            "Unmapped property",
            "Property '{0}' in destination type '{1}' has no corresponding source property",
            "Mapping",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        private static readonly DiagnosticDescriptor TypeMismatchRule = new(
            TypeMismatchDiagnosticId,
            "Type mismatch in mapping",
            "Cannot map '{0}.{1}' ({2}) to '{3}.{4}' ({5})",
            "Mapping",
            DiagnosticSeverity.Error,
            isEnabledByDefault: true);

        // Новое правило: если свойство Destination не имеет маппинга и не игнорируется
        private static readonly DiagnosticDescriptor MissingMappingRule = new(
            MissingMappingDiagnosticId,
            "Missing property mapping",
            "Property '{0}' in destination type '{1}' is not mapped in CreateMap configuration",
            "Mapping",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
            ImmutableArray.Create(UnmappedPropertyRule, TypeMismatchRule, MissingMappingRule);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            // Регистрируем обработчик вызовов
            context.RegisterOperationAction(AnalyzeInvocation, OperationKind.Invocation);
        }

        private void AnalyzeInvocation(OperationAnalysisContext context)
        {
            if (context.Operation is not IInvocationOperation invocation ||
                !IsCreateMapMethod(invocation.TargetMethod))
            {
                return;
            }

            // Получаем ВСЕ маппинги в решении
            allMappings = GetAllMappingsInSolution(context.Compilation);

            // Анализируем текущий CreateMap
            ProcessCreateMap(context, invocation, allMappings);
        }
        
        private bool IsCreateMapMethod(IMethodSymbol method)
        {
            return method.Name.Contains("CreateMap") && 
                   method.TypeArguments.Length == 2;
        }

        private void ProcessCreateMap(
            OperationAnalysisContext context,
            IInvocationOperation invocation,
            List<(ITypeSymbol Source, ITypeSymbol Destination)> allMappings)
        {
            if (invocation.TargetMethod is not IMethodSymbol methodSymbol ||
                methodSymbol.TypeArguments.Length != 2)
            {
                return;
            }

            var sourceType = methodSymbol.TypeArguments[0];
            var destinationType = methodSymbol.TypeArguments[1];
            var chain = GetFullInvocationChain(invocation);

            // 1. Проверяем прямой маппинг
            CheckPropertyMappings(context, sourceType, destinationType,
                invocation.Syntax.GetLocation(), chain, allMappings);

            // 2. Проверяем обратный маппинг, если есть ReverseMap
            if (chain.Any(op => op.TargetMethod.Name == "ReverseMap"))
            {
                CheckPropertyMappings(context, destinationType, sourceType,
                    invocation.Syntax.GetLocation(), chain, allMappings, isReverse: true);
            }
        }

        private List<(ITypeSymbol Source, ITypeSymbol Destination)> GetAllMappingsInSolution(Compilation compilation)
        {
            var mappings = new List<(ITypeSymbol Source, ITypeSymbol Destination)>();

            foreach (var syntaxTree in compilation.SyntaxTrees)
            {
                var semanticModel = compilation.GetSemanticModel(syntaxTree);
                var root = syntaxTree.GetRoot();

                foreach (var node in root.DescendantNodes().OfType<InvocationExpressionSyntax>())
                {
                    if (node.Expression is GenericNameSyntax memberAccess &&
                        memberAccess.Identifier.Text.Contains("CreateMap"))
                    {
                        var operation = semanticModel.GetOperation(node) as IInvocationOperation;
                        if (operation != null && operation.TargetMethod is IMethodSymbol method &&
                            method.TypeArguments.Length == 2)
                        {
                            var sourceType = method.TypeArguments[0];
                            var destinationType = method.TypeArguments[1];

                            if (!mappings.Any(m =>
                                    SymbolEqualityComparer.Default.Equals(m.Source, sourceType) &&
                                    SymbolEqualityComparer.Default.Equals(m.Destination, destinationType)))
                            {
                                mappings.Add((sourceType, destinationType));
                            }

                            // Проверяем ReverseMap
                            var parent = operation.Parent as IInvocationOperation;
                            while (parent != null)
                            {
                                if (parent.TargetMethod.Name == "ReverseMap")
                                {
                                    if (!mappings.Any(m =>
                                            SymbolEqualityComparer.Default.Equals(m.Source, destinationType) &&
                                            SymbolEqualityComparer.Default.Equals(m.Destination, sourceType)))
                                    {
                                        mappings.Add((destinationType, sourceType));
                                    }

                                    break;
                                }

                                parent = parent.Parent as IInvocationOperation;
                            }
                        }
                    }
                }
            }
            return mappings;
        }



        private bool IsCreateMapInvocation(InvocationExpressionSyntax invocation)
        {
            return invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                   memberAccess.Name.Identifier.Text.Contains("CreateMap");
        }

        private void AddMappingIfNotExists(
            List<(ITypeSymbol Source, ITypeSymbol Destination)> mappings,
            ITypeSymbol sourceType,
            ITypeSymbol destinationType)
        {
            if (!mappings.Any(m =>
                    SymbolEqualityComparer.Default.Equals(m.Source, sourceType) &&
                    SymbolEqualityComparer.Default.Equals(m.Destination, destinationType)))
            {
                mappings.Add((sourceType, destinationType));
            }
        }

        // Проверяем, используется ли ForMember или Ignore для маппинга свойств
        private bool CheckExplicitMappings(IInvocationOperation invocation)
        {
            // Если вызов — часть цепочки (например, CreateMap<...>().ForMember(...))
            if (invocation.Parent is IExpressionStatementOperation expressionStatement &&
                expressionStatement.Operation is IInvocationOperation parentInvocation)
            {
                // Проверяем, есть ли ForMember или Ignore
                return parentInvocation.TargetMethod.Name switch
                {
                    "ForMember" or "Ignore" or "ForPatch" => true,
                    _ => false
                };
            }

            return false;
        }

        private void CheckPropertyMappings(
            OperationAnalysisContext context,
            ITypeSymbol sourceType,
            ITypeSymbol destinationType,
            Location location,
            List<IInvocationOperation> invocationChain,
            List<(ITypeSymbol Source, ITypeSymbol Destination)> allMappings,
            bool isReverse = false)
        {
            var sourceProperties = sourceType.GetMembers()
                .OfType<IPropertySymbol>()
                .Where(p => !p.IsReadOnly)
                .ToList();

            var destinationProperties = destinationType.GetMembers()
                .OfType<IPropertySymbol>()
                .Where(p => !p.IsReadOnly)
                .ToList();

            var mappingInfo = GetExplicitMappingsInfo(invocationChain);

            foreach (var destProp in destinationProperties)
            {
                if (mappingInfo.MappedProperties.Contains(destProp.Name, StringComparer.OrdinalIgnoreCase))
                    continue;

                if (mappingInfo.IgnoredProperties.Contains(destProp.Name, StringComparer.OrdinalIgnoreCase))
                    continue;

                var sourceProp = sourceProperties.FirstOrDefault(p =>
                    string.Equals(p.Name, destProp.Name, StringComparison.OrdinalIgnoreCase));

                if (sourceProp == null)
                {
                    // Check if property type has mapping
                    if (!HasPropertyTypeMapping(destProp.Type, allMappings))
                    {
                        context.ReportDiagnostic(Diagnostic.Create(
                            MissingMappingRule,
                            location,
                            destProp.Name,
                            destinationType.Name));
                    }
                }
                else 
                {
                    // Skip type checking for properties with FromMember, FromPath, or explicit mapping
                    var hasExplicitMapping = invocationChain.Any(op => 
                        op.TargetMethod.Name is "ForMember" or "ForPath" && 
                        GetMappedPropertyName(op)?.Equals(destProp.Name, StringComparison.OrdinalIgnoreCase) == true);
                    
                    if (!hasExplicitMapping && !IsTypeCompatible(sourceProp.Type, destProp.Type))
                    {
                        if (!HasTypeMapping(sourceProp.Type, destProp.Type, allMappings) && 
                            !IsCollectionWithMappedElementType(destProp.Type, allMappings))
                        {
                            context.ReportDiagnostic(Diagnostic.Create(
                                TypeMismatchRule,
                                location,
                                sourceType.Name,
                                sourceProp.Name,
                                sourceProp.Type.Name,
                                destinationType.Name,
                                destProp.Name,
                                destProp.Type.Name));
                        }
                    }
                }
            }
        }
        
        private bool HasPropertyTypeMapping(ITypeSymbol propertyType, 
            List<(ITypeSymbol Source, ITypeSymbol Destination)> allMappings)
        {
            return allMappings.Any(m => 
                SymbolEqualityComparer.Default.Equals(m.Source, propertyType));
        }
        
        private bool IsCollectionWithMappedElementType(ITypeSymbol propertyType, 
            List<(ITypeSymbol Source, ITypeSymbol Destination)> allMappings)
        {
            // Если это не коллекции - сразу false
            if (!IsCollectionType(propertyType))
                return false;

            // Получаем типы элементов коллекций
            var propertyElementType = GetCollectionElementType(propertyType);
            
            return HasPropertyTypeMapping(propertyElementType, allMappings);
        }
        
        // Определяет, является ли тип коллекцией
        private bool IsCollectionType(ITypeSymbol type)
        {
            return type is IArrayTypeSymbol || 
                   type.AllInterfaces.Any(i => 
                       i.SpecialType == SpecialType.System_Collections_Generic_ICollection_T ||
                       i.SpecialType == SpecialType.System_Collections_IEnumerable);
        }
        
        // Получает тип элементов коллекции
        private ITypeSymbol GetCollectionElementType(ITypeSymbol type)
        {
            switch (type)
            {
                case IArrayTypeSymbol arrayType:
                    return arrayType.ElementType;
                case INamedTypeSymbol namedType when namedType.IsGenericType:
                    return namedType.TypeArguments.FirstOrDefault();
                default:
                    return type;
            }
        }
        private void AnalyzeInvocationCreateMappings(OperationAnalysisContext context)
        {
            if (context.Operation is not IInvocationOperation invocation ||
                !invocation.TargetMethod.Name.Contains("CreateMap"))
            {
                return;
            }

            if (invocation.TargetMethod is not IMethodSymbol methodSymbol ||
                methodSymbol.TypeArguments.Length != 2)
            {
                return;
            }

            var sourceType = methodSymbol.TypeArguments[0];
            var destinationType = methodSymbol.TypeArguments[1];

            var chain = GetFullInvocationChain(invocation);

            if (!allMappings.Any(m =>
                    SymbolEqualityComparer.Default.Equals(m.Source, sourceType) &&
                    SymbolEqualityComparer.Default.Equals(m.Destination, destinationType)))
            {
                allMappings.Add((sourceType, destinationType));
            }

            // Если есть ReverseMap() — тоже добавим обратный маппинг
            if (chain.Any(op => op.TargetMethod.Name == "ReverseMap") &&
                !allMappings.Any(m =>
                    SymbolEqualityComparer.Default.Equals(m.Source, destinationType) &&
                    SymbolEqualityComparer.Default.Equals(m.Destination, sourceType)))
            {
                allMappings.Add((destinationType, sourceType));
            }
        }

        private void AnalyzeInvocationMappingRules(OperationAnalysisContext context)
        {
            if (context.Operation is not IInvocationOperation invocation ||
                !invocation.TargetMethod.Name.Contains("CreateMap"))
            {
                return;
            }

            if (invocation.TargetMethod is not IMethodSymbol methodSymbol ||
                methodSymbol.TypeArguments.Length != 2)
            {
                return;
            }

            var sourceType = methodSymbol.TypeArguments[0];
            var destinationType = methodSymbol.TypeArguments[1];
            var chain = GetFullInvocationChain(invocation);

            // Прямой маппинг
            CheckPropertyMappings(context, sourceType, destinationType, 
                invocation.Syntax.GetLocation(), chain, allMappings);

            // Если есть ReverseMap() — ищем маппинг в обратном направлении
            if (chain.Any(op => op.TargetMethod.Name == "ReverseMap"))
            {
                CheckPropertyMappings(context, destinationType, sourceType, 
                    invocation.Syntax.GetLocation(), chain, allMappings, isReverse: true);
            }
        }


        private List<IInvocationOperation> GetFullInvocationChain(IInvocationOperation start)
        {
            var chain = new List<IInvocationOperation> { start };

            var current = start.Parent as IInvocationOperation;
            while (current != null)
            {
                chain.Add(current);
                current = current.Parent as IInvocationOperation;
            }

            chain.Reverse(); // чтобы шло от CreateMap к концу
            return chain;
        }

        private MappingInfo GetExplicitMappingsInfo(List<IInvocationOperation> chain)
        {
            var info = new MappingInfo();

            foreach (var op in chain)
            {
                var methodName = op.TargetMethod.Name;

                if (methodName is "ForMember" or "ForPatch" or "ForPath")
                {
                    var propertyName = GetMappedPropertyName(op);
                    if (!string.IsNullOrEmpty(propertyName))
                    {
                        if (HasIgnoreCall(op))
                        {
                            info.IgnoredProperties.Add(propertyName);
                        }
                        else
                        {
                            info.MappedProperties.Add(propertyName);
                        }
                    }
                }
                else if (methodName == "Ignore")
                {
                    var propertyName = GetIgnoredPropertyName(op);
                    if (!string.IsNullOrEmpty(propertyName))
                    {
                        info.IgnoredProperties.Add(propertyName);
                    }
                }
            }

            return info;
        }

        private bool HasIgnoreCall(IInvocationOperation forMemberOperation)
        {
            if (forMemberOperation.Arguments.Length < 2)
                return false;

            // Анализируем синтаксис второго аргумента
            var optionsSyntax = (forMemberOperation.Syntax as InvocationExpressionSyntax)
                ?.ArgumentList.Arguments[1].Expression;

            // Проверяем наличие вызова Ignore()
            return optionsSyntax is InvocationExpressionSyntax invocation &&
                   (invocation.Expression as MemberAccessExpressionSyntax)?.Name.Identifier.Text == "Ignore";
        }

        private string GetIgnoredPropertyName(IInvocationOperation operation)
        {
            if (operation.Arguments.Length == 0)
                return null;

            // Получаем синтаксис лямбда-выражения
            if (operation.Arguments[0].Syntax is LambdaExpressionSyntax lambdaSyntax)
            {
                return GetPropertyNameFromLambda(lambdaSyntax);
            }

            return null;
        }

        private string GetMappedPropertyName(IInvocationOperation operation)
        {
            if (operation.TargetMethod?.Name != "ForMember" &&
                operation.TargetMethod?.Name != "ForPath")
                return null;

            // Получаем синтаксическое представление вызова
            var invocationSyntax = operation.Syntax as InvocationExpressionSyntax;
            if (invocationSyntax?.ArgumentList.Arguments.Count == 0)
                return null;

            // Анализируем первый аргумент (лямбда-выражение)
            var firstArg = invocationSyntax.ArgumentList.Arguments[0];
            return GetPropertyNameFromExpression(firstArg.Expression);
        }

        private string GetPropertyNameFromExpression(ExpressionSyntax expression)
        {
            // Обрабатываем разные формы лямбда-выражений
            switch (expression)
            {
                case SimpleLambdaExpressionSyntax simpleLambda:
                    return GetPropertyNameFromLambdaBody(simpleLambda.Body);

                case ParenthesizedLambdaExpressionSyntax parenLambda:
                    return GetPropertyNameFromLambdaBody(parenLambda.Body);

                default:
                    return null;
            }
        }

        private string GetPropertyNameFromLambdaBody(CSharpSyntaxNode body)
        {
            switch (body)
            {
                // Простая форма: dest => dest.Property
                case MemberAccessExpressionSyntax memberAccess:
                    return memberAccess.Name.Identifier.Text;

                // Форма с блоком: dest => { return dest.Property; }
                case BlockSyntax block:
                    var returnStatement = block.Statements
                        .OfType<ReturnStatementSyntax>()
                        .FirstOrDefault();
                    if (returnStatement?.Expression is MemberAccessExpressionSyntax returnMember)
                        return returnMember.Name.Identifier.Text;

                    var exprStatement = block.Statements
                        .OfType<ExpressionStatementSyntax>()
                        .FirstOrDefault();
                    if (exprStatement?.Expression is AssignmentExpressionSyntax assignment)
                        return (assignment.Left as MemberAccessExpressionSyntax)?.Name.Identifier.Text;
                    break;

                // Форма с присваиванием: dest => dest.Property = value
                case AssignmentExpressionSyntax assignment1:
                    return (assignment1.Left as MemberAccessExpressionSyntax)?.Name.Identifier.Text;
            }

            return null;
        }

        private string GetPropertyNameFromLambda(LambdaExpressionSyntax lambdaSyntax)
        {
            // Обрабатываем разные формы лямбда-выражений
            switch (lambdaSyntax.Body)
            {
                // Простая форма: dest => dest.Property
                case MemberAccessExpressionSyntax memberAccess:
                    return memberAccess.Name.Identifier.Text;

                // Форма с блоком: dest => { return dest.Property; }
                case BlockSyntax block when block.Statements.FirstOrDefault() is ReturnStatementSyntax returnStatement:
                    return (returnStatement.Expression as MemberAccessExpressionSyntax)?.Name.Identifier.Text ??
                           string.Empty;

                // Форма с присваиванием: dest => dest.Property = value
                case AssignmentExpressionSyntax assignment:
                    return (assignment.Left as MemberAccessExpressionSyntax)?.Name.Identifier.Text ?? string.Empty;

                // Форма с блоком и присваиванием: dest => { dest.Property = value; }
                case BlockSyntax block
                    when block.Statements.FirstOrDefault() is ExpressionStatementSyntax exprStatement:
                    return ((exprStatement.Expression as AssignmentExpressionSyntax)?.Left is
                        MemberAccessExpressionSyntax leftMember
                            ? leftMember.Name.Identifier.Text
                            : null) ?? string.Empty;

                default:
                    return null;
            }
        }

        private bool HasTypeMapping(ITypeSymbol sourceType, ITypeSymbol destType, 
            List<(ITypeSymbol Source, ITypeSymbol Destination)> allMappings)
        {
            return allMappings.Any(m =>
                SymbolEqualityComparer.Default.Equals(m.Source, sourceType) &&
                SymbolEqualityComparer.Default.Equals(m.Destination, destType));
        }

        private bool IsTypeCompatible(ITypeSymbol sourceType, ITypeSymbol destType)
        {
            // Handle nullable to non-nullable conversion
            var sourceUnderlyingType = (sourceType as INamedTypeSymbol)?.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T 
                ? (sourceType as INamedTypeSymbol).TypeArguments[0] 
                : sourceType;
    
            var destUnderlyingType = (destType as INamedTypeSymbol)?.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T 
                ? (destType as INamedTypeSymbol).TypeArguments[0] 
                : destType;

            // Check underlying types
            if (SymbolEqualityComparer.Default.Equals(sourceUnderlyingType, destUnderlyingType))
                return true;

            if (IsNumericType(sourceUnderlyingType) && IsNumericType(destUnderlyingType))
                return true;

            if ((sourceUnderlyingType.Name == "Guid" && destUnderlyingType.SpecialType == SpecialType.System_String) ||
                (sourceUnderlyingType.SpecialType == SpecialType.System_String && destUnderlyingType.Name == "Guid"))
                return true;

            return false;
        }

        private bool IsNumericType(ITypeSymbol type)
        {
            // (прежняя реализация)
            return type.SpecialType switch
            {
                SpecialType.System_Byte or
                    SpecialType.System_SByte or
                    SpecialType.System_Int16 or
                    SpecialType.System_UInt16 or
                    SpecialType.System_Int32 or
                    SpecialType.System_UInt32 or
                    SpecialType.System_Int64 or
                    SpecialType.System_UInt64 or
                    SpecialType.System_Single or
                    SpecialType.System_Double or
                    SpecialType.System_Decimal => true,
                _ => false
            };
        }


    }
}