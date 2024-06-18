---@class Ast
local Ast = {}

---@alias NODETYPE
---| NODETYPE_STAT
---| NODETYPE_EXPR
---| "UnknownNode"

---@alias NODETYPE_STAT
---| "DoStatNode"
---| "IfStatNode"
---| "WhileStatNode"
---| "IterativeForStatNode"
---| "NumericForStatNode"
---| "RepeatStatNode"
---| "FunctionDeclarationStatNode"
---| "LocalFunctionDeclarationStatNode"
---| "VariableAssignmentStatNode"
---| "LocalVariableAssignmentStatNode"
---| "ReturnStatNode"
---| "BreakStatNode"
---| "FunctionCallStatNode"
---| "GotoLabelStatNode"
---| "GotoStatNode"

---@alias NODETYPE_LITERAL
---| "NumberLiteralExpressionNode"
---| "StringLiteralExpressionNode"
---| "NilLiteralExpressionNode"
---| "BooleanLiteralExpressionNode"
---| "VarargLiteralExpressionNode"

---@alias NODETYPE_EXPR
---| NODETYPE_LITERAL
---| "TableConstructorExpressionNode"
---| "FunctionCallExpressionNode"
---| "OpExpressionNode"
---| "BinaryOpExpressionNode"
---| "UnaryOpExpressionNode"
---| "NameExpressionNode"

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
---@field [integer] StatNode
---@field filename string


---@class StatNode : Node
Ast.StatNode = Ast.Node:extend("UnknownNode")

---@class DoStatNode : StatNode
---@field type "DoStatNode"
---@field block StatNode[]
Ast.DoStatNode = Ast.StatNode:extend("DoStatNode")

---@class _AtomicIf
---@field condition ExpressionNode
---@field block StatNode[]

---@class IfStatNode : StatNode
---@field type "IfStatNode"
---@field ifNodes _AtomicIf[]
---@field elseBlock StatNode[]?
Ast.IfStatNode = Ast.StatNode:extend("IfStatNode")

---@class WhileStatNode : StatNode
---@field type "WhileStatNode"
---@field condition ExpressionNode
---@field block StatNode[]
Ast.WhileStatNode = Ast.StatNode:extend("WhileStatNode")

---@class IterativeForStatNode : StatNode
---@field type "IterativeForStatNode"
---@field names string[]
---@field expressions ExpressionNode[]
---@field block StatNode[]
Ast.IterativeForStatNode = Ast.StatNode:extend("IterativeForStatNode")

---@class NumericForStatNode : StatNode
---@field type "NumericForStatNode"
---@field name string
---@field startExpr ExpressionNode
---@field endExpr ExpressionNode
---@field stepExpr ExpressionNode?
---@field block StatNode[]
Ast.NumericForStatNode = Ast.StatNode:extend("NumericForStatNode")

---@class RepeatStatNode : StatNode
---@field type "RepeatStatNode"
---@field block StatNode[]
---@field untilCondition ExpressionNode
Ast.RepeatStatNode = Ast.StatNode:extend("RepeatStatNode")

---@class FunctionDeclarationStatNode : StatNode
---@field type "FunctionDeclarationStatNode"
---@field name string
---@field parameters string[]
---@field block StatNode[]
Ast.FunctionDeclarationStatNode = Ast.StatNode:extend("FunctionDeclarationStatNode")

---@class LocalFunctionStatNode : FunctionDeclarationStatNode
---@field type "LocalFunctionDeclarationStatNode"
Ast.LocalFunctionDeclarationStatNode = Ast.FunctionDeclarationStatNode:extend("LocalFunctionDeclarationStatNode")

---@class VariableAssignmentStatNode : StatNode
---@field type "VariableAssignmentStatNode"
---@field names string[]
---@field expressions ExpressionNode[]
Ast.VariableAssignmentStatNode = Ast.StatNode:extend("VariableAssignmentStatNode")

---@class LocalVariableDeclarationStatNode : VariableAssignmentStatNode
---@field type "LocalVariableAssignmentStatNode"
Ast.LocalVariableAssignmentStatNode = Ast.VariableAssignmentStatNode:extend("LocalVariableAssignmentStatNode")

---@class ReturnStatNode : StatNode
---@field type "ReturnStatNode"
---@field expressions ExpressionNode[]
Ast.ReturnStatNode = Ast.StatNode:extend("ReturnStatNode")

---@class BreakStatNode : StatNode
---@field type "BreakStatNode"
Ast.BreakStatNode = Ast.StatNode:extend("BreakStatNode")

---@class FunctionCallStatNode : StatNode
---@field type "FunctionCallStatNode"
---@field expression FunctionCallExpressionNode
Ast.FunctionCallStatNode = Ast.StatNode:extend("FunctionCallStatNode")

---@class GotoLabelStatNode : StatNode
---@field type "GotoLabelStatNode"
---@field name string
Ast.GotoLabelStatNode = Ast.StatNode:extend("GotoLabelStatNode")

---@class GotoStatNode : StatNode
---@field type "GotoStatNode"
---@field name string
Ast.GotoStatNode = Ast.StatNode:extend("GotoStatNode")


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

---@class VarargLiteralExpressionNode : LiteralExpressionNode
---@field value { kind: "..." }
Ast.VarargLiteralExpressionNode = Ast.LiteralExpressionNode:extend("VarargLiteralExpressionNode")

---@class _TableNode
---@field key ExpressionNode
---@field value ExpressionNode

---@class TableConstructorExpressionNode : ExpressionNode
---@field fields _TableNode[]
Ast.TableConstructorExpressionNode = Ast.ExpressionNode:extend("TableConstructorExpressionNode")

---@class FunctionCallExpressionNode : ExpressionNode
---@field target ExpressionNode
---@field method string?
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

---@class NameExpressionNode : ExpressionNode
---@field name string | "..."
Ast.NameExpressionNode = Ast.ExpressionNode:extend("NameExpressionNode")

return Ast