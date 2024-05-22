---@meta

---@class Node
---@field type string

---@class StatementNode : Node

---@class _AtomicIfNode : Node
---@field condition ExpressionNode
---@field block StatementNode[]

---@class IfStatementNode : StatementNode
---@field ifNodes _AtomicIfNode[]
---@field elseBlock StatementNode[]?

---@class WhileStatementNode : StatementNode
---@field condition ExpressionNode
---@field block StatementNode[]

---@class IterativeForStatementNode : StatementNode
---@field names TokenType.NAME[]
---@field expressions ExpressionNode[]
---@field block StatementNode[]

---@class NumericForStatementNode : StatementNode
---@field name TokenType.NAME
---@field start ExpressionNode
---@field end ExpressionNode
---@field step ExpressionNode?
---@field block StatementNode[]

---@class RepeatStatementNode : StatementNode
---@field block StatementNode[]
---@field untilCondition ExpressionNode

---@class FunctionDeclarationStatementNode : StatementNode
---@field name TokenType.NAME
---@field parameters TokenType.NAME[]
---@field block StatementNode[]

---@class LocalFunctionStatementNode : FunctionDeclarationStatementNode

---@class VariableAssignmentStatementNode : StatementNode
---@field names TokenType.NAME[]
---@field expressions ExpressionNode[]

---@class LocalVariableDeclarationStatementNode : StatementNode

---@class ReturnStatementNode : StatementNode
---@field expressions ExpressionNode[]

---@class BreakStatementNode : StatementNode




---@class ExpressionNode : Node

