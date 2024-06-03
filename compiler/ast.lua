local ast = {}

---@alias NODETYPE
---| NODETYPE_STMT
---| NODETYPE_EXPR
---| "UnknownNode"

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
---| "ReturnStatementNode"
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
---| "BinaryOpExpressionNode"
---| "UnaryOpExpressionNode"

do
    ---@class Node
    ---@field type NODETYPE
    ---@field protected _subtypes table<NODETYPE, boolean>
    ast.Node = {
        type = "UnknownNode",
        _subtypes = {},
    }

    function ast.Node:__tostring()
        return self.type
    end

    --- If given node has the same nodetype with this, return the given node. Otherwise, return nil.
    --- This is useful for type checking.
    ---@generic T: Node
    ---@param self T
    ---@param node Node
    ---@return T | nil
    function ast.Node:check(node)
        ---@diagnostic disable-next-line: undefined-field
        if self.type == node.type or self._subtypes[node.type] then
            return node
        else
            return nil
        end
    end

    ---@param data table?
    ---@return self
    function ast.Node:new(data)
        assert(self.type ~= "UnknownNode", "Cannot create an instance of UnknownNode")
        return setmetatable(data or {}, {__index = self, __tostring = self.__tostring})
    end

    --- Returns a new table that extends this node.
    ---@param type NODETYPE
    ---@return self
    function ast.Node:extend(type)
        self:_addSubtype(type)
        return setmetatable({type = type, _subtypes = {}}, {__index = self, __tostring = self.__tostring})
    end

    ---@protected
    ---@param type NODETYPE
    function ast.Node:_addSubtype(type)
        self._subtypes[type] = true
        local mt = getmetatable(self)
        if mt and mt._addSubtype then
            mt:_addSubtype(type)
        end
    end
end


---@class StatementNode : Node
ast.StatementNode = ast.Node:extend("UnknownNode")

---@class DoStatementNode : StatementNode
---@field type "DoStatementNode"
---@field block StatementNode[]
ast.DoStatementNode = ast.StatementNode:extend("DoStatementNode")

---@class _AtomicIf
---@field condition ExpressionNode
---@field block StatementNode[]

---@class IfStatementNode : StatementNode
---@field type "IfStatementNode"
---@field ifNodes _AtomicIf[]
---@field elseBlock StatementNode[]?
ast.IfStatementNode = ast.StatementNode:extend("IfStatementNode")

---@class WhileStatementNode : StatementNode
---@field type "WhileStatementNode"
---@field condition ExpressionNode
---@field block StatementNode[]
ast.WhileStatementNode = ast.StatementNode:extend("WhileStatementNode")

---@class IterativeForStatementNode : StatementNode
---@field type "IterativeForStatementNode"
---@field names TokenType.NAME[]
---@field expressions ExpressionNode[]
---@field block StatementNode[]
ast.IterativeForStatementNode = ast.StatementNode:extend("IterativeForStatementNode")

---@class NumericForStatementNode : StatementNode
---@field type "NumericForStatementNode"
---@field name TokenType.NAME
---@field start ExpressionNode
---@field end ExpressionNode
---@field step ExpressionNode?
---@field block StatementNode[]
ast.NumericForStatementNode = ast.StatementNode:extend("NumericForStatementNode")

---@class RepeatStatementNode : StatementNode
---@field type "RepeatStatementNode"
---@field block StatementNode[]
---@field untilCondition ExpressionNode
ast.RepeatStatementNode = ast.StatementNode:extend("RepeatStatementNode")

---@class FunctionDeclarationStatementNode : StatementNode
---@field type "FunctionDeclarationStatementNode"
---@field name TokenType.NAME
---@field parameters TokenType.NAME[]
---@field block StatementNode[]
ast.FunctionDeclarationStatementNode = ast.StatementNode:extend("FunctionDeclarationStatementNode")

---@class LocalFunctionStatementNode : FunctionDeclarationStatementNode
---@field type "LocalFunctionDeclarationStatementNode"
ast.LocalFunctionDeclarationStatementNode = ast.FunctionDeclarationStatementNode:extend("LocalFunctionDeclarationStatementNode")

---@class VariableAssignmentStatementNode : StatementNode
---@field type "VariableAssignmentStatementNode"
---@field names TokenType.NAME[]
---@field expressions ExpressionNode[]
ast.VariableAssignmentStatementNode = ast.StatementNode:extend("VariableAssignmentStatementNode")

---@class LocalVariableDeclarationStatementNode : StatementNode
---@field type "LocalVariableAssignmentStatementNode"
ast.LocalVariableAssignmentStatementNode = ast.StatementNode:extend("LocalVariableAssignmentStatementNode")

---@class ReturnStatementNode : StatementNode
---@field type "ReturnStatementNode"
---@field expressions ExpressionNode[]
ast.ReturnStatementNode = ast.StatementNode:extend("ReturnStatementNode")

---@class BreakStatementNode : StatementNode
---@field type "BreakStatementNode"
ast.BreakStatementNode = ast.StatementNode:extend("BreakStatementNode")

---@class FunctionCallStatementNode : StatementNode
---@field type "FunctionCallStatementNode"
---@field expression ExpressionNode
ast.FunctionCallStatementNode = ast.StatementNode:extend("FunctionCallStatementNode")

---@class GotoLabelStatementNode : StatementNode
---@field type "GotoLabelStatementNode"
---@field name string
ast.GotoLabelStatementNode = ast.StatementNode:extend("GotoLabelStatementNode")

---@class GotoStatementNode : StatementNode
---@field type "GotoStatementNode"
---@field name string
ast.GotoStatementNode = ast.StatementNode:extend("GotoStatementNode")


---@class ExpressionNode : Node
ast.ExpressionNode = ast.Node:extend("UnknownNode")

---@class LiteralExpressionNode : ExpressionNode
---@field value any
ast.LiteralExpressionNode = ast.ExpressionNode:extend("UnknownNode")

---@class NumberLiteralExpressionNode : LiteralExpressionNode
---@field value number
ast.NumberLiteralExpressionNode = ast.LiteralExpressionNode:extend("NumberLiteralExpressionNode")

---@class StringLiteralExpressionNode : LiteralExpressionNode
---@field value string
ast.StringLiteralExpressionNode = ast.LiteralExpressionNode:extend("StringLiteralExpressionNode")

---@class NilLiteralExpressionNode : LiteralExpressionNode
---@field value nil
ast.NilLiteralExpressionNode = ast.LiteralExpressionNode:extend("NilLiteralExpressionNode")

---@class BooleanLiteralExpressionNode : LiteralExpressionNode
---@field value boolean
ast.BooleanLiteralExpressionNode = ast.LiteralExpressionNode:extend("BooleanLiteralExpressionNode")

---@class _TableNode
---@field key ExpressionNode
---@field value ExpressionNode

---@class TableLiteralExpressionNode : LiteralExpressionNode
---@field fields _TableNode[]
ast.TableLiteralExpressionNode = ast.LiteralExpressionNode:extend("TableLiteralExpressionNode")

---@class FunctionCallExpressionNode : ExpressionNode
---@field name ExpressionNode
---@field args ExpressionNode[]
ast.FunctionCallExpressionNode = ast.ExpressionNode:extend("FunctionCallExpressionNode")

---@class BinaryOpExpressionNode : ExpressionNode
---@field operator string
---@field left ExpressionNode
---@field right ExpressionNode
ast.BinaryOpExpressionNode = ast.ExpressionNode:extend("BinaryOpExpressionNode")

---@class UnaryOpExpressionNode : ExpressionNode
---@field operator string
---@field expression ExpressionNode
ast.UnaryOpExpressionNode = ast.ExpressionNode:extend("UnaryOpExpressionNode")


return ast