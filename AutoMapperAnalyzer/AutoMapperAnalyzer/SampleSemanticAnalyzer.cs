using System;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace AutoMapperAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class AutoMapperAnalyzer : DiagnosticAnalyzer
    {
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
            context.RegisterOperationAction(AnalyzeInvocation, OperationKind.Invocation);
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
                    "ForMember" or "Ignore" => true,
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
    bool hasExplicitMappings)
    {
        var sourceProperties = sourceType.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(p => !p.IsReadOnly)
            .ToList();
        
        var destinationProperties = destinationType.GetMembers()
            .OfType<IPropertySymbol>()
            .Where(p => !p.IsReadOnly)
            .ToList();

        foreach (var destProp in destinationProperties)
        {
            var sourceProp = sourceProperties.FirstOrDefault(p => 
                string.Equals(p.Name, destProp.Name, StringComparison.OrdinalIgnoreCase));

            if (sourceProp == null)
            {
                // Property exists only in destination type
                context.ReportDiagnostic(Diagnostic.Create(
                    UnmappedPropertyRule,
                    location,
                    destProp.Name,
                    destinationType.Name));

                if (!hasExplicitMappings)
                {
                    context.ReportDiagnostic(Diagnostic.Create(
                        MissingMappingRule,
                        location,
                        destProp.Name,
                        destinationType.Name));
                }
            }
            else if (!IsTypeCompatible(sourceProp.Type, destProp.Type))
            {
                // Type mismatch
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

    private void AnalyzeInvocation(OperationAnalysisContext context)
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

        // Проверяем всю цепочку вызовов на наличие явных маппингов
        var hasExplicitMappings = CheckExplicitMappingsInChain(invocation);
        var isReverseMap = CheckForReverseMap(invocation);

        // Проверяем прямой маппинг
        CheckPropertyMappings(context, sourceType, destinationType, 
            invocation.Syntax.GetLocation(), hasExplicitMappings);

        // Если есть ReverseMap, проверяем обратный маппинг
        if (isReverseMap)
        {
            CheckPropertyMappings(context, destinationType, sourceType, 
                invocation.Syntax.GetLocation(), hasExplicitMappings);
        }
    }

    private bool CheckExplicitMappingsInChain(IInvocationOperation invocation)
    {
        // Проверяем всю цепочку вызовов
        var currentOperation = invocation.Parent as IInvocationOperation;
        while (currentOperation != null)
        {
            if (currentOperation.TargetMethod.Name == "ForMember" || 
                currentOperation.TargetMethod.Name == "Ignore")
            {
                return true;
            }
            currentOperation = currentOperation.Parent as IInvocationOperation;
        }
        return false;
    }

    private bool CheckForReverseMap(IInvocationOperation invocation)
    {
        var currentOperation = invocation.Parent as IInvocationOperation;
        while (currentOperation != null)
        {
            if (currentOperation.TargetMethod.Name == "ReverseMap")
            {
                return true;
            }
            currentOperation = currentOperation.Parent as IInvocationOperation;
        }
        return false;
    }



        private bool IsTypeCompatible(ITypeSymbol sourceType, ITypeSymbol destType)
        {
            // (прежняя реализация)
            if (SymbolEqualityComparer.Default.Equals(sourceType, destType))
                return true;

            if (IsNumericType(sourceType) && IsNumericType(destType))
                return true;

            if ((sourceType.Name == "Guid" && destType.SpecialType == SpecialType.System_String) ||
                (sourceType.SpecialType == SpecialType.System_String && destType.Name == "Guid"))
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