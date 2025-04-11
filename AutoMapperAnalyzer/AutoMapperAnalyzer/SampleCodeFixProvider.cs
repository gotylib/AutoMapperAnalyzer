using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace AutoMapperAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(AutoMapperCodeFixProvider)), Shared]
    public class AutoMapperCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds => 
            ImmutableArray.Create(AutoMapperAnalyzer.UnmappedPropertyDiagnosticId, AutoMapperAnalyzer.TypeMismatchDiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider() => WellKnownFixAllProviders.BatchFixer;

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            
            foreach (var diagnostic in context.Diagnostics)
            {
                var diagnosticSpan = diagnostic.Location.SourceSpan;

                // Находим узел CreateMap
                var createMapInvocation = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf()
                    .OfType<InvocationExpressionSyntax>().FirstOrDefault();

                if (createMapInvocation == null)
                    continue;

                if (diagnostic.Id == AutoMapperAnalyzer.UnmappedPropertyDiagnosticId)
                {
                    context.RegisterCodeFix(
                        CodeAction.Create(
                            title: "Add mapping for property",
                            createChangedDocument: c => AddPropertyMappingAsync(context.Document, createMapInvocation, diagnostic, c),
                            equivalenceKey: "AddPropertyMapping"),
                        diagnostic);
                }
                else if (diagnostic.Id == AutoMapperAnalyzer.TypeMismatchDiagnosticId)
                {
                    context.RegisterCodeFix(
                        CodeAction.Create(
                            title: "Add custom type converter",
                            createChangedDocument: c => AddTypeConverterAsync(context.Document, createMapInvocation, diagnostic, c),
                            equivalenceKey: "AddTypeConverter"),
                        diagnostic);
                }
            }
        }

        private async Task<Document> AddPropertyMappingAsync(Document document, InvocationExpressionSyntax createMapInvocation, 
            Diagnostic diagnostic, CancellationToken cancellationToken)
        {
            var propertyName = diagnostic.Properties["propertyName"];
            
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            
            // Создаем новое выражение ForMember
            var forMemberExpression = SyntaxFactory.ParseExpression(
                $".ForMember(dest => dest.{propertyName}, opt => opt.MapFrom(src => src.{propertyName}))");

            // Добавляем новое выражение к существующему CreateMap
            var newCreateMapInvocation = createMapInvocation.WithExpression(
                ((MemberAccessExpressionSyntax)createMapInvocation.Expression).WithName(
                    ((MemberAccessExpressionSyntax)createMapInvocation.Expression).Name
                    .WithIdentifier(SyntaxFactory.Identifier("CreateMap"))));

            var newRoot = root.ReplaceNode(createMapInvocation, newCreateMapInvocation);
            return document.WithSyntaxRoot(newRoot);
        }

        private async Task<Document> AddTypeConverterAsync(Document document, InvocationExpressionSyntax createMapInvocation,
            Diagnostic diagnostic, CancellationToken cancellationToken)
        {
            var sourceProperty = diagnostic.Properties["sourceProperty"];
            var destProperty = diagnostic.Properties["destProperty"];
            
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            
            // Создаем выражение ConvertUsing с примером преобразования
            var convertUsingExpression = SyntaxFactory.ParseExpression(
                $".ConvertUsing(src => {destProperty}Convert(src.{sourceProperty}))");

            var newCreateMapInvocation = createMapInvocation.WithExpression(
                ((MemberAccessExpressionSyntax)createMapInvocation.Expression).WithName(
                    ((MemberAccessExpressionSyntax)createMapInvocation.Expression).Name
                    .WithIdentifier(SyntaxFactory.Identifier("CreateMap"))));

            var newRoot = root.ReplaceNode(createMapInvocation, newCreateMapInvocation);
            return document.WithSyntaxRoot(newRoot);
        }
    }
}