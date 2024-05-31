---@meta

---@alias NODETYPE
---| NODETYPE_STMT
---| NODETYPE_EXPR

---@alias NODETYPE_STMT
---| "DoStatementNode"
---| "IfStatementNode"
---| "WhileStatementNode"
---| "IterativeForStatementNode"
---| "NumericForStatementNode"
---| "RepeatStatementNode"
---| "FunctionDeclarationStatementNode"
---| "LocalFunctionDeclarationStatementNode"
---| "VariableAssignmentStatementNode"
---| "LocalVariableAssignmentStatementNode"
---| "BreakStatementNode"
---| "FunctionCallStatementNode"
---| "GotoLabelStatementNode"
---| "GotoStatementNode"

---@alias NODETYPE_LITERAL
---| "NumberLiteralExpressionNode"
---| "StringLiteralExpressionNode"
---| "NilLiteralExpressionNode"
---| "BooleanLiteralExpressionNode"
---| "TableLiteralExpressionNode"

---@alias NODETYPE_EXPR
---| NODETYPE_LITERAL
---| "FunctionCallExpressionNode"
---| "OpExpressionNode"
---| "UnaryOpExpressionNode"
---| "BinaryOpExpressionNode"


---@class Node
---@field type NODETYPE

---@class StatementNode : Node

---@class DoStatementNode : StatementNode
---@field type "DoStatementNode"
---@field block StatementNode[]

---@class _AtomicIf
---@field condition ExpressionNode
---@field block StatementNode[]

---@class IfStatementNode : StatementNode
---@field type "IfStatementNode"
---@field ifNodes _AtomicIf[]
---@field elseBlock StatementNode[]?

---@class WhileStatementNode : StatementNode
---@field type "WhileStatementNode"
---@field condition ExpressionNode
---@field block StatementNode[]

---@class IterativeForStatementNode : StatementNode
---@field type "IterativeForStatementNode"
---@field names TokenType.NAME[]
---@field expressions ExpressionNode[]
---@field block StatementNode[]

---@class NumericForStatementNode : StatementNode
---@field type "NumericForStatementNode"
---@field name TokenType.NAME
---@field start ExpressionNode
---@field end ExpressionNode
---@field step ExpressionNode?
---@field block StatementNode[]

---@class RepeatStatementNode : StatementNode
---@field type "RepeatStatementNode"
---@field block StatementNode[]
---@field untilCondition ExpressionNode

---@class FunctionDeclarationStatementNode : StatementNode
---@field type "FunctionDeclarationStatementNode"
---@field name TokenType.NAME
---@field parameters TokenType.NAME[]
---@field block StatementNode[]

---@class LocalFunctionStatementNode : FunctionDeclarationStatementNode
---@field type "LocalFunctionDeclarationStatementNode"

---@class VariableAssignmentStatementNode : StatementNode
---@field type "VariableAssignmentStatementNode"
---@field names TokenType.NAME[]
---@field expressions ExpressionNode[]

---@class LocalVariableDeclarationStatementNode : StatementNode
---@field type "LocalVariableAssignmentStatementNode"

---@class ReturnStatementNode : StatementNode
---@field type "ReturnStatementNode"
---@field expressions ExpressionNode[]

---@class BreakStatementNode : StatementNode
---@field type "BreakStatementNode"

---@class FunctionCallStatementNode : StatementNode
---@field type "FunctionCallStatementNode"
---@field expression ExpressionNode

---@class GotoLabelStatementNode : StatementNode
---@field type "GotoLabelStatementNode"
---@field name string

---@class GotoStatementNode : StatementNode
---@field type "GotoStatementNode"
---@field name string


---@class ExpressionNode : Node

---@class LiteralExpressionNode : ExpressionNode
---@field value any

---@class NumberLiteralExpressionNode : LiteralExpressionNode
---@field value number

---@class StringLiteralExpressionNode : LiteralExpressionNode
---@field value string

---@class NilLiteralExpressionNode : LiteralExpressionNode
---@field value nil

---@class BooleanLiteralExpressionNode : LiteralExpressionNode
---@field value boolean

---@class _TableNode
---@field key ExpressionNode
---@field value ExpressionNode

---@class TableLiteralExpressionNode : LiteralExpressionNode
---@field fields _TableNode[]

---@class FunctionCallExpressionNode : ExpressionNode
---@field name ExpressionNode
---@field args ExpressionNode[]

---@class BinaryOpExpressionNode : ExpressionNode
---@field operator string
---@field left ExpressionNode
---@field right ExpressionNode

---@class UnaryOpExpressionNode : ExpressionNode
---@field operator string
---@field expression ExpressionNode

