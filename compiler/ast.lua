local Ast = {}

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
    ---@field position { line: number, column: number }
    ---@field protected _subtypes table<NODETYPE, boolean>
    Ast.Node = {
        type = "UnknownNode",
        _subtypes = {},
    }

    function Ast.Node:__tostring()
        return self.type
    end

    --- If given node has the same nodetype with this, return the given node. Otherwise, return nil.
    --- This is useful for type checking.
    ---@generic T: Node
    ---@param self T
    ---@param node Node
    ---@return T | nil
    function Ast.Node:check(node)
        ---@diagnostic disable-next-line: undefined-field
        if self.type == node.type or self._subtypes[node.type] then
            return node
        else
            return nil
        end
    end

    ---@generic T: Node
    ---@param self T
    ---@param data table?
    ---@param position { line: number, column: number }?
    ---@return T
    function Ast.Node:new(data, position)
        ---@diagnostic disable-next-line: undefined-field
        assert(self.type ~= "UnknownNode", "Cannot create an instance of UnknownNode")

        data = data or {}
        data.position = position or { line = 0, column = 0 }

        ---@diagnostic disable-next-line: undefined-field
        return setmetatable(data, {__index = self, __tostring = self.__tostring})
    end

    --- Returns a new table that extends this node.
    ---@param type NODETYPE
    ---@return self
    function Ast.Node:extend(type)
        self:_addSubtype(type)
        return setmetatable({type = type, _subtypes = {}}, {__index = self, __tostring = self.__tostring})
    end

    ---@protected
    ---@param type NODETYPE
    function Ast.Node:_addSubtype(type)
        self._subtypes[type] = true
        local mt = getmetatable(self)
        if mt and mt._addSubtype then
            mt:_addSubtype(type)
        end
    end
end

---@class AbstractSyntaxTree
---@field [integer] StatementNode
---@field filename string


---@class StatementNode : Node
Ast.StatementNode = Ast.Node:extend("UnknownNode")

---@class DoStatementNode : StatementNode
---@field type "DoStatementNode"
---@field block StatementNode[]
Ast.DoStatementNode = Ast.StatementNode:extend("DoStatementNode")

---@class _AtomicIf
---@field condition ExpressionNode
---@field block StatementNode[]

---@class IfStatementNode : StatementNode
---@field type "IfStatementNode"
---@field ifNodes _AtomicIf[]
---@field elseBlock StatementNode[]?
Ast.IfStatementNode = Ast.StatementNode:extend("IfStatementNode")

---@class WhileStatementNode : StatementNode
---@field type "WhileStatementNode"
---@field condition ExpressionNode
---@field block StatementNode[]
Ast.WhileStatementNode = Ast.StatementNode:extend("WhileStatementNode")

---@class IterativeForStatementNode : StatementNode
---@field type "IterativeForStatementNode"
---@field names string[]
---@field expressions ExpressionNode[]
---@field block StatementNode[]
Ast.IterativeForStatementNode = Ast.StatementNode:extend("IterativeForStatementNode")

---@class NumericForStatementNode : StatementNode
---@field type "NumericForStatementNode"
---@field name string
---@field start ExpressionNode
---@field end ExpressionNode
---@field step ExpressionNode?
---@field block StatementNode[]
Ast.NumericForStatementNode = Ast.StatementNode:extend("NumericForStatementNode")

---@class RepeatStatementNode : StatementNode
---@field type "RepeatStatementNode"
---@field block StatementNode[]
---@field untilCondition ExpressionNode
Ast.RepeatStatementNode = Ast.StatementNode:extend("RepeatStatementNode")

---@class FunctionDeclarationStatementNode : StatementNode
---@field type "FunctionDeclarationStatementNode"
---@field name string
---@field parameters string[]
---@field block StatementNode[]
Ast.FunctionDeclarationStatementNode = Ast.StatementNode:extend("FunctionDeclarationStatementNode")

---@class LocalFunctionStatementNode : FunctionDeclarationStatementNode
---@field type "LocalFunctionDeclarationStatementNode"
Ast.LocalFunctionDeclarationStatementNode = Ast.FunctionDeclarationStatementNode:extend("LocalFunctionDeclarationStatementNode")

---@class VariableAssignmentStatementNode : StatementNode
---@field type "VariableAssignmentStatementNode"
---@field names string[]
---@field expressions ExpressionNode[]
Ast.VariableAssignmentStatementNode = Ast.StatementNode:extend("VariableAssignmentStatementNode")

---@class LocalVariableDeclarationStatementNode : StatementNode
---@field type "LocalVariableAssignmentStatementNode"
Ast.LocalVariableAssignmentStatementNode = Ast.StatementNode:extend("LocalVariableAssignmentStatementNode")

---@class ReturnStatementNode : StatementNode
---@field type "ReturnStatementNode"
---@field expressions ExpressionNode[]
Ast.ReturnStatementNode = Ast.StatementNode:extend("ReturnStatementNode")

---@class BreakStatementNode : StatementNode
---@field type "BreakStatementNode"
Ast.BreakStatementNode = Ast.StatementNode:extend("BreakStatementNode")

---@class FunctionCallStatementNode : StatementNode
---@field type "FunctionCallStatementNode"
---@field expression ExpressionNode
Ast.FunctionCallStatementNode = Ast.StatementNode:extend("FunctionCallStatementNode")

---@class GotoLabelStatementNode : StatementNode
---@field type "GotoLabelStatementNode"
---@field name string
Ast.GotoLabelStatementNode = Ast.StatementNode:extend("GotoLabelStatementNode")

---@class GotoStatementNode : StatementNode
---@field type "GotoStatementNode"
---@field name string
Ast.GotoStatementNode = Ast.StatementNode:extend("GotoStatementNode")


---@class ExpressionNode : Node
Ast.ExpressionNode = Ast.Node:extend("UnknownNode")

---@class LiteralExpressionNode : ExpressionNode
---@field value any
Ast.LiteralExpressionNode = Ast.ExpressionNode:extend("UnknownNode")

---@class NumberLiteralExpressionNode : LiteralExpressionNode
---@field value number
Ast.NumberLiteralExpressionNode = Ast.LiteralExpressionNode:extend("NumberLiteralExpressionNode")

---@class StringLiteralExpressionNode : LiteralExpressionNode
---@field value string
Ast.StringLiteralExpressionNode = Ast.LiteralExpressionNode:extend("StringLiteralExpressionNode")

---@class NilLiteralExpressionNode : LiteralExpressionNode
---@field value nil
Ast.NilLiteralExpressionNode = Ast.LiteralExpressionNode:extend("NilLiteralExpressionNode")

---@class BooleanLiteralExpressionNode : LiteralExpressionNode
---@field value boolean
Ast.BooleanLiteralExpressionNode = Ast.LiteralExpressionNode:extend("BooleanLiteralExpressionNode")

---@class _TableNode
---@field key ExpressionNode
---@field value ExpressionNode

---@class TableLiteralExpressionNode : LiteralExpressionNode
---@field fields _TableNode[]
Ast.TableLiteralExpressionNode = Ast.LiteralExpressionNode:extend("TableLiteralExpressionNode")

---@class FunctionCallExpressionNode : ExpressionNode
---@field name ExpressionNode
---@field args ExpressionNode[]
Ast.FunctionCallExpressionNode = Ast.ExpressionNode:extend("FunctionCallExpressionNode")

---@class BinaryOpExpressionNode : ExpressionNode
---@field operator string
---@field left ExpressionNode
---@field right ExpressionNode
Ast.BinaryOpExpressionNode = Ast.ExpressionNode:extend("BinaryOpExpressionNode")

---@class UnaryOpExpressionNode : ExpressionNode
---@field operator string
---@field expression ExpressionNode
Ast.UnaryOpExpressionNode = Ast.ExpressionNode:extend("UnaryOpExpressionNode")


return Ast