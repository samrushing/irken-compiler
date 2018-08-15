;; -*- Mode: Irken -*-

;; This grammar was originally converted automatically (by antlr2irken) from
;; https://github.com/antlr/grammars-v4/blob/master/c/C.g4
;;
;; However several tweaks were made, mostly to accommodate the 'lex hack'

(define c-grammar
  (first
   (%%sexp
    (grammar
     ;;(compilationUnit EOF (translationUnit EOF))
     (translationUnit externalDeclaration (translationUnit externalDeclaration))
     (externalDeclaration functionDefinition declaration typeRedefinition Semi)
     ;; added to catch redefinitions, which will otherwise not parse.
     (typeRedefinition
      (Typedef declarationSpecifiers TypedefName))
     (primaryExpression
      Identifier
      Constant
      primaryExpression-0
      (LeftParen expression RightParen)
      genericSelection
      (LeftParen compoundStatement RightParen)
      (Extension LeftParen compoundStatement RightParen)
      (BuiltinVaArg LeftParen unaryExpression Comma typeName RightParen)
      (BuiltinOffsetof LeftParen typeName Comma unaryExpression RightParen))
     (genericSelection (Generic LeftParen assignmentExpression Comma genericAssocList RightParen))
     (genericAssocList genericAssociation (genericAssocList Comma genericAssociation))
     (genericAssociation (typeName Colon assignmentExpression) (Default Colon assignmentExpression))
     (postfixExpression
      primaryExpression
      (postfixExpression LeftBracket expression RightBracket)
      (postfixExpression LeftParen RightParen)
      (postfixExpression LeftParen argumentExpressionList RightParen)
      (postfixExpression Dot Identifier)
      (postfixExpression Arrow Identifier)
      (postfixExpression PlusPlus)
      (postfixExpression MinusMinus)
      (LeftParen typeName RightParen LeftBrace initializerList RightBrace)
      (LeftParen typeName RightParen LeftBrace initializerList Comma RightBrace)
      (Extension LeftParen typeName RightParen LeftBrace initializerList RightBrace)
      (Extension LeftParen typeName RightParen LeftBrace initializerList Comma RightBrace))
     (argumentExpressionList assignmentExpression (argumentExpressionList Comma assignmentExpression))
     (unaryExpression
      postfixExpression
      (PlusPlus unaryExpression)
      (MinusMinus unaryExpression)
      (unaryOperator castExpression)
      (Sizeof unaryExpression)
      (Sizeof LeftParen typeName RightParen)
      (Alignof LeftParen typeName RightParen)
      (AndAnd Identifier))
     (unaryOperator And Star Plus Minus Tilde Not)
     (castExpression
      (LeftParen typeName RightParen castExpression)
      (Extension LeftParen typeName RightParen castExpression)
      unaryExpression
      DigitSequence
      )
     (multiplicativeExpression
      castExpression
      (multiplicativeExpression Star castExpression)
      (multiplicativeExpression Div castExpression)
      (multiplicativeExpression Mod castExpression))
     (additiveExpression
      multiplicativeExpression
      (additiveExpression Plus multiplicativeExpression)
      (additiveExpression Minus multiplicativeExpression))
     (shiftExpression
      additiveExpression
      (shiftExpression LeftShift additiveExpression)
      (shiftExpression RightShift additiveExpression))
     (relationalExpression
      shiftExpression
      (relationalExpression Less shiftExpression)
      (relationalExpression Greater shiftExpression)
      (relationalExpression LessEqual shiftExpression)
      (relationalExpression GreaterEqual shiftExpression))
     (equalityExpression
      relationalExpression
      (equalityExpression Equal relationalExpression)
      (equalityExpression NotEqual relationalExpression))
     (andExpression equalityExpression (andExpression And equalityExpression))
     (exclusiveOrExpression andExpression (exclusiveOrExpression Caret andExpression))
     (inclusiveOrExpression exclusiveOrExpression (inclusiveOrExpression Or exclusiveOrExpression))
     (logicalAndExpression inclusiveOrExpression (logicalAndExpression AndAnd inclusiveOrExpression))
     (logicalOrExpression logicalAndExpression (logicalOrExpression OrOr logicalAndExpression))
     (conditionalExpression (logicalOrExpression condExprTail) logicalOrExpression)
     (condExprTail (Question expression Colon conditionalExpression))
     (assignmentExpression
      conditionalExpression
      (unaryExpression assignmentOperator assignmentExpression)
      DigitSequence)
     (assignmentOperator
      Assign
      StarAssign
      DivAssign
      ModAssign
      PlusAssign
      MinusAssign
      LeftShiftAssign
      RightShiftAssign
      AndAssign
      XorAssign
      OrAssign)
     (expression assignmentExpression (expression Comma assignmentExpression))
     (constantExpression conditionalExpression)
     (declaration
      (declarationSpecifiers initDeclaratorList Semi)
      (declarationSpecifiers Semi)
      staticAssertDeclaration)
     (declarationSpecifiers declarationSpecifiers-0)
     (declarationSpecifiers2 declarationSpecifiers2-0)
     (declarationSpecifier
      storageClassSpecifier
      typeSpecifier
      typeQualifier
      functionSpecifier
      alignmentSpecifier)
     (initDeclaratorList initDeclarator (initDeclaratorList Comma initDeclarator))
     (initDeclarator declarator (declarator Assign initializer))
     (storageClassSpecifier Typedef Extern Static ThreadLocal Auto Register)
     (typeSpecifier
      BaseTypes
      (Extension LeftParen M128 RightParen)
      atomicTypeSpecifier
      structOrUnionSpecifier
      enumSpecifier
      ;;typedefName
      TypedefName ;; NOTE: this is a terminal, from frob-typedef-names
      (TypeOf LeftParen constantExpression RightParen)
      (typeSpecifier pointer))
     ;; added for the 'lex hack' - sometimes in 'struct xxx', 'xxx' is also typedef'd.
     (identifier Identifier TypedefName)
     (structOrUnionSpecifier
      (structOrUnion LeftBrace structDeclarationList RightBrace)
      (structOrUnion identifier LeftBrace structDeclarationList RightBrace)
      (structOrUnion identifier))
     (structOrUnion Struct Union)
     (structDeclarationList structDeclaration (structDeclarationList structDeclaration))
     (structDeclaration
      (specifierQualifierList Semi)
      (specifierQualifierList structDeclaratorList Semi)
      staticAssertDeclaration)
     (specifierQualifierList
      typeSpecifier
      (typeSpecifier specifierQualifierList)
      typeQualifier
      (typeQualifier specifierQualifierList))
     (structDeclaratorList structDeclarator (structDeclaratorList Comma structDeclarator))
     (structDeclarator declarator (Colon constantExpression) (declarator Colon constantExpression))
     (enumSpecifier
      (Enum LeftBrace enumeratorList RightBrace)
      (Enum Identifier LeftBrace enumeratorList RightBrace)
      (Enum LeftBrace enumeratorList Comma RightBrace)
      (Enum Identifier LeftBrace enumeratorList Comma RightBrace)
      (Enum Identifier))
     (enumeratorList enumerator (enumeratorList Comma enumerator))
     (enumerator enumerationConstant (enumerationConstant Assign constantExpression))
     (enumerationConstant Identifier)
     (atomicTypeSpecifier (Atomic LeftParen typeName RightParen))
     (typeQualifier Const Restrict Volatile Atomic)
     (functionSpecifier FunSpec gccAttributeSpecifier (DeclSpec LeftParen Identifier RightParen))
     (alignmentSpecifier
      (Alignas LeftParen typeName RightParen)
      (Alignas LeftParen constantExpression RightParen))
     (declarator
      directDeclarator
      (directDeclarator declarator-0)
      (pointer directDeclarator)
      (pointer directDeclarator declarator-0))
     (directDeclarator
      Identifier
      (LeftParen declarator RightParen)
      (directDeclarator LeftBracket RightBracket)
      (directDeclarator LeftBracket assignmentExpression RightBracket)
      (directDeclarator LeftBracket typeQualifierList RightBracket)
      (directDeclarator LeftBracket typeQualifierList assignmentExpression RightBracket)
      (directDeclarator LeftBracket Static assignmentExpression RightBracket)
      (directDeclarator LeftBracket Static typeQualifierList assignmentExpression RightBracket)
      (directDeclarator LeftBracket typeQualifierList Static assignmentExpression RightBracket)
      (directDeclarator LeftBracket Star RightBracket)
      (directDeclarator LeftBracket typeQualifierList Star RightBracket)
      (directDeclarator LeftParen parameterTypeList RightParen)
      (directDeclarator LeftParen RightParen)
      (directDeclarator LeftParen identifierList RightParen)
      (Identifier Colon DigitSequence)
      (LeftParen pointer directDeclarator RightParen)
      (LeftParen typeSpecifier pointer directDeclarator RightParen))
     (gccDeclaratorExtension
      (Asm LeftParen gccDeclaratorExtension-0 RightParen)
      gccAttributeSpecifier)
     (gccAttributeSpecifier (Attribute LeftParen LeftParen gccAttributeList RightParen RightParen))
     (gccAttributeList gccAttribute (gccAttribute gccAttributeList-1))
     (gccAttribute gccAttrIdOrReserved (gccAttrIdOrReserved gccAttr1))
     (gccAttr1 (LeftParen RightParen) (LeftParen argumentExpressionList RightParen))
     (gccAttrIdOrReserved Identifier Const)
     (pointer
      Star
      (Star typeQualifierList)
      (Star pointer)
      (Star typeQualifierList pointer)
      Caret
      (Caret typeQualifierList)
      (Caret pointer)
      (Caret typeQualifierList pointer))
     (typeQualifierList typeQualifier (typeQualifierList typeQualifier))
     (parameterTypeList parameterList (parameterList Comma Ellipsis))
     (parameterList parameterDeclaration (parameterList Comma parameterDeclaration))
     (parameterDeclaration
      (declarationSpecifiers declarator)
      declarationSpecifiers2
      (declarationSpecifiers2 abstractDeclarator))
     (identifierList Identifier (identifierList Comma Identifier))
     (typeName specifierQualifierList (specifierQualifierList abstractDeclarator))
     (abstractDeclarator
      pointer
      directAbstractDeclarator
      (directAbstractDeclarator abstractDeclarator-0)
      (pointer directAbstractDeclarator)
      (pointer directAbstractDeclarator abstractDeclarator-0))
     (directAbstractDeclarator
      (LeftParen abstractDeclarator RightParen)
      (LeftParen abstractDeclarator RightParen directAbstractDeclarator-0)
      (LeftBracket RightBracket)
      (LeftBracket assignmentExpression RightBracket)
      (LeftBracket typeQualifierList RightBracket)
      (LeftBracket typeQualifierList assignmentExpression RightBracket)
      (LeftBracket Static assignmentExpression RightBracket)
      (LeftBracket Static typeQualifierList assignmentExpression RightBracket)
      (LeftBracket typeQualifierList Static assignmentExpression RightBracket)
      (LeftBracket Star RightBracket)
      (LeftParen RightParen)
      (LeftParen RightParen directAbstractDeclarator-0)
      (LeftParen parameterTypeList RightParen)
      (LeftParen parameterTypeList RightParen directAbstractDeclarator-0)
      (directAbstractDeclarator LeftBracket RightBracket)
      (directAbstractDeclarator LeftBracket assignmentExpression RightBracket)
      (directAbstractDeclarator LeftBracket typeQualifierList RightBracket)
      (directAbstractDeclarator LeftBracket typeQualifierList assignmentExpression RightBracket)
      (directAbstractDeclarator LeftBracket Static assignmentExpression RightBracket)
      (directAbstractDeclarator LeftBracket Static typeQualifierList assignmentExpression RightBracket)
      (directAbstractDeclarator LeftBracket typeQualifierList Static assignmentExpression RightBracket)
      (directAbstractDeclarator LeftBracket Star RightBracket)
      (directAbstractDeclarator LeftParen RightParen)
      (directAbstractDeclarator LeftParen RightParen directAbstractDeclarator-0)
      (directAbstractDeclarator LeftParen parameterTypeList RightParen)
      (directAbstractDeclarator LeftParen parameterTypeList RightParen directAbstractDeclarator-0))
     (typedefName Identifier)
     (initializer
      assignmentExpression
      (LeftBrace initializerList RightBrace)
      (LeftBrace initializerList Comma RightBrace))
     (initializerList
      initializer
      (designation initializer)
      (initializerList Comma initializer)
      (initializerList Comma designation initializer))
     (designation (designatorList Assign))
     (designatorList designator (designatorList designator))
     (designator (LeftBracket constantExpression RightBracket) (Dot Identifier))
     (staticAssertDeclaration
      (StaticAssert LeftParen constantExpression Comma staticAssertDeclaration-0 RightParen Semi))
     (statement
      labeledStatement
      compoundStatement
      expressionStatement
      selectionStatement
      iterationStatement
      jumpStatement)
     (labeledStatement
      (Identifier Colon statement)
      (Case constantExpression Colon statement)
      (Default Colon statement))
     (compoundStatement (LeftBrace RightBrace) (LeftBrace blockItemList RightBrace))
     (blockItemList blockItem (blockItemList blockItem))
     (blockItem statement declaration)
     (expressionStatement Semi (expression Semi))
     (selectionStatement
      (If LeftParen expression RightParen statement)
      (If LeftParen expression RightParen statement selectionStatement-0)
      (If LeftParen expression RightParen statement Else statement)
      (Switch LeftParen expression RightParen statement))
     (iterationStatement
      (While LeftParen expression RightParen statement)
      (Do statement While LeftParen expression RightParen Semi)
      (For LeftParen forCondition RightParen statement))
     (forCondition
      (forDeclaration Semi Semi)
      (forDeclaration Semi Semi forExpression)
      (forDeclaration Semi forExpression Semi)
      (forDeclaration Semi forExpression Semi forExpression)
      (Semi Semi)
      (Semi Semi forExpression)
      (Semi forExpression Semi)
      (Semi forExpression Semi forExpression)
      (expression Semi Semi)
      (expression Semi Semi forExpression)
      (expression Semi forExpression Semi)
      (expression Semi forExpression Semi forExpression))
     (forDeclaration (declarationSpecifiers initDeclaratorList) declarationSpecifiers)
     (forExpression assignmentExpression (forExpression Comma assignmentExpression))
     (jumpStatement
      (Goto Identifier Semi)
      (Continue Semi)
      (Break Semi)
      (Return Semi)
      (Return expression Semi)
      (Goto unaryExpression Semi))
     (functionDefinition
      (declarator compoundStatement)
      (declarator declarationList compoundStatement)
      (declarationSpecifiers declarator compoundStatement)
      (declarationSpecifiers declarator declarationList compoundStatement))
     (declarationList declaration (declarationList declaration))
     (primaryExpression-0 (primaryExpression-0 StringLiteral) StringLiteral)
     (declarationSpecifiers-0 (declarationSpecifiers-0 declarationSpecifier) declarationSpecifier)
     (declarationSpecifiers2-0 (declarationSpecifiers2-0 declarationSpecifier) declarationSpecifier)
     (declarator-0 (declarator-0 gccDeclaratorExtension) gccDeclaratorExtension)
     (gccDeclaratorExtension-0 (gccDeclaratorExtension-0 StringLiteral) StringLiteral)
     (gccAttributeList-0 (Comma gccAttribute))
     (gccAttributeList-1 (gccAttributeList-1 gccAttributeList-0) gccAttributeList-0)
     (abstractDeclarator-0 (abstractDeclarator-0 gccDeclaratorExtension) gccDeclaratorExtension)
     (directAbstractDeclarator-0
      (directAbstractDeclarator-0 gccDeclaratorExtension)
      gccDeclaratorExtension)
     (directAbstractDeclarator-0
      (directAbstractDeclarator-0 gccDeclaratorExtension)
      gccDeclaratorExtension)
     (directAbstractDeclarator-0
      (directAbstractDeclarator-0 gccDeclaratorExtension)
      gccDeclaratorExtension)
     (staticAssertDeclaration-0 (staticAssertDeclaration-0 StringLiteral) StringLiteral)
     (selectionStatement-0 (Else statement)))
    )))

