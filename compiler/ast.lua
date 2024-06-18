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
---| "NumberLiteralExpNode"
---| "StringLiteralExpNode"
---| "NilLiteralExpNode"
---| "BooleanLiteralExpNode"

---@alias NODETYPE_EXPR
---| NODETYPE_LITERAL
---| "TableConstructorExpNode"
---| "FunctionCallExpNode"
---| "OpExpNode"
---| "BinaryOpExpNode"
---| "UnaryOpExpNode"
---| "NameExpNode"
---| "TableIndexExpNode"
---| "FunctionDefExpNode"

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
---@field condition ExpNode
---@field block StatNode[]

---@class IfStatNode : StatNode
---@field type "IfStatNode"
---@field ifNodes _AtomicIf[]
---@field elseBlock StatNode[]?
Ast.IfStatNode = Ast.StatNode:extend("IfStatNode")

---@class WhileStatNode : StatNode
---@field type "WhileStatNode"
---@field condition ExpNode
---@field block StatNode[]
Ast.WhileStatNode = Ast.StatNode:extend("WhileStatNode")

---@class IterativeForStatNode : StatNode
---@field type "IterativeForStatNode"
---@field names string[]
---@field expressions ExpNode[]
---@field block StatNode[]
Ast.IterativeForStatNode = Ast.StatNode:extend("IterativeForStatNode")

---@class NumericForStatNode : StatNode
---@field type "NumericForStatNode"
---@field name string
---@field startExpr ExpNode
---@field endExpr ExpNode
---@field stepExpr ExpNode?
---@field block StatNode[]
Ast.NumericForStatNode = Ast.StatNode:extend("NumericForStatNode")

---@class RepeatStatNode : StatNode
---@field type "RepeatStatNode"
---@field block StatNode[]
---@field untilCondition ExpNode
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
---@field expressions ExpNode[]
Ast.VariableAssignmentStatNode = Ast.StatNode:extend("VariableAssignmentStatNode")

---@class LocalVariableDeclarationStatNode : VariableAssignmentStatNode
---@field type "LocalVariableAssignmentStatNode"
Ast.LocalVariableAssignmentStatNode = Ast.VariableAssignmentStatNode:extend("LocalVariableAssignmentStatNode")

---@class ReturnStatNode : StatNode
---@field type "ReturnStatNode"
---@field expressions ExpNode[]
Ast.ReturnStatNode = Ast.StatNode:extend("ReturnStatNode")

---@class BreakStatNode : StatNode
---@field type "BreakStatNode"
Ast.BreakStatNode = Ast.StatNode:extend("BreakStatNode")

---@class FunctionCallStatNode : StatNode
---@field type "FunctionCallStatNode"
---@field expression FunctionCallExpNode
Ast.FunctionCallStatNode = Ast.StatNode:extend("FunctionCallStatNode")

---@class GotoLabelStatNode : StatNode
---@field type "GotoLabelStatNode"
---@field name string
Ast.GotoLabelStatNode = Ast.StatNode:extend("GotoLabelStatNode")

---@class GotoStatNode : StatNode
---@field type "GotoStatNode"
---@field name string
Ast.GotoStatNode = Ast.StatNode:extend("GotoStatNode")


---@class ExpNode : Node
Ast.ExpNode = Ast.Node:extend("UnknownNode")

---@class LiteralExpNode : ExpNode
---@field value any
Ast.LiteralExpNode = Ast.ExpNode:extend("UnknownNode")

---@class NumberLiteralExpNode : LiteralExpNode
---@field type "NumberLiteralExpNode"
---@field value number
Ast.NumberLiteralExpNode = Ast.LiteralExpNode:extend("NumberLiteralExpNode")

---@class StringLiteralExpNode : LiteralExpNode
---@field type "StringLiteralExpNode"
---@field value string
Ast.StringLiteralExpNode = Ast.LiteralExpNode:extend("StringLiteralExpNode")

---@class NilLiteralExpNode : LiteralExpNode
---@field type "NilLiteralExpNode"
---@field value nil
Ast.NilLiteralExpNode = Ast.LiteralExpNode:extend("NilLiteralExpNode")

---@class BooleanLiteralExpNode : LiteralExpNode
---@field type "BooleanLiteralExpNode"
---@field value boolean
Ast.BooleanLiteralExpNode = Ast.LiteralExpNode:extend("BooleanLiteralExpNode")

---@class _TableNode
---@field key ExpNode
---@field value ExpNode

---@class TableConstructorExpNode : ExpNode
---@field type "TableConstructorExpNode"
---@field fields _TableNode[]
Ast.TableConstructorExpNode = Ast.ExpNode:extend("TableConstructorExpNode")

---@class FunctionCallExpNode : ExpNode
---@field type "FunctionCallExpNode"
---@field target ExpNode
---@field method string?
---@field args ExpNode[]
Ast.FunctionCallExpNode = Ast.ExpNode:extend("FunctionCallExpNode")

---@class BinaryOpExpNode : ExpNode
---@field type "BinaryOpExpNode"
---@field operator string
---@field left ExpNode
---@field right ExpNode
Ast.BinaryOpExpNode = Ast.ExpNode:extend("BinaryOpExpNode")

---@class UnaryOpExpNode : ExpNode
---@field type "UnaryOpExpNode"
---@field operator string
---@field expression ExpNode
Ast.UnaryOpExpNode = Ast.ExpNode:extend("UnaryOpExpNode")

---@class NameExpNode : ExpNode
---@field type "NameExpNode"
---@field name string | "..."
Ast.NameExpNode = Ast.ExpNode:extend("NameExpNode")

---@class TableIndexExpNode : ExpNode
---@field type "TableIndexExpNode"
---@field target ExpNode
---@field key ExpNode
Ast.TableIndexExpNode= Ast.ExpNode:extend("TableIndexExpNode")

---@class FunctionDefExpNode : ExpNode
---@field type "FunctionDefExpNode"
---@field parameters string[]
---@field block StatNode[]
Ast.FunctionDefExpNode = Ast.ExpNode:extend("FunctionDefExpNode")


return Ast