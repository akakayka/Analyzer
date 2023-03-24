using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Analyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(AnalyzerCodeFixProvider)), Shared]
    public class AnalyzerCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(VarAnalyzer.DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            if (root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ForEachStatementSyntax>().Count() != 0)
            {
                var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ForEachStatementSyntax>().First();
                Func<ForEachStatementSyntax, TypeSyntax, ForEachStatementSyntax> WithType = (x, type) => x.WithType(type);

                context.RegisterCodeFix(
                CodeAction.Create(
                    title: CodeFixResources.CodeFixTitle,
                createChangedDocument: c => MakeVarAsync(context.Document, declaration, c, WithType),
                equivalenceKey: nameof(CodeFixResources.CodeFixTitle)),
                diagnostic);
            }
            else
            {
                Func<VariableDeclarationSyntax, TypeSyntax, VariableDeclarationSyntax> WithType = (x, type) => x.WithType(type);
                var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<VariableDeclarationSyntax>().First();

                context.RegisterCodeFix(
                CodeAction.Create(
                    title: CodeFixResources.CodeFixTitle,
                createChangedDocument: c => MakeVarAsync<VariableDeclarationSyntax>(context.Document, declaration, c, WithType),
                equivalenceKey: nameof(CodeFixResources.CodeFixTitle)),
                diagnostic);
            }

        }

        private static async Task<Document> MakeVarAsync<T>(Document document,
    T variableDeclaration,
    CancellationToken cancellationToken,
    Func<T, TypeSyntax, T> WithType)
            where T: SyntaxNode
        {
            SyntaxToken firstToken = variableDeclaration.GetFirstToken();
            T trimmedLocal = variableDeclaration.ReplaceToken(
                firstToken, firstToken.WithLeadingTrivia(SyntaxTriviaList.Empty));

            TypeSyntax newType = SyntaxFactory.ParseTypeName("ауе");
            T newLocal = WithType(trimmedLocal,newType);
            T formattedLocal = newLocal.WithAdditionalAnnotations(Formatter.Annotation);

            SyntaxNode oldRoot = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            SyntaxNode newRoot = oldRoot.ReplaceNode(variableDeclaration, formattedLocal);

            return document.WithSyntaxRoot(newRoot);
        }
        
    }

    
}
