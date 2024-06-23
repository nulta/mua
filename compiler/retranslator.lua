-- This module re-translates the AST to Lua code.

local Ast = require("compiler.ast")


---@class Retrans
local Retrans = {}


---@param node FunctionCallExpNode | FunctionCallStatNode
function Retrans:parseFunctionCallNode(node)
    local code = {self:parse(node.target)}
    if node.method then
        table.insert(code, ":")
        table.insert(code, node.method)
    end
    table.insert(code, "(")
    table.insert(code, self:parseMultiple(node.args, ","))
    table.insert(code, ")")
    return code
end


---@type table<NODETYPE, string[] | fun(Retrans, Node): string[]>
Retrans.nodeSwitches = {
    ["DoStatNode"]
    = {"do", "{block}", "end"},

    ["IfStatNode"] ---@param node IfStatNode
    = function(self, node)
        local code = {}

        for k, ifNode in ipairs(node.ifNodes) do
            table.insert(code, (k == 1) and "if" or "elseif")
            table.insert(code, self:parse(ifNode.condition))
            table.insert(code, "then")
            table.insert(code, self:parseMultiple(ifNode.block))
        end

        if node.elseBlock then
            table.insert(code, "else")
            table.insert(code, self:parseMultiple(node.elseBlock))
        end

        table.insert(code, "end")

        return code
    end,

    ["WhileStatNode"]
    = {"while", "{condition}", "do", "{block}", "end"},

    ["IterativeForStatNode"]
    = {"for", "{names,}", "in", "{expressions}", "do", "{block}", "end"},

    ["NumericForStatNode"] ---@param node NumericForStatNode
    = function(self, node)
        local code = {"for"}

        table.insert(code, node.name)
        table.insert(code, "=")
        table.insert(code, self:parse(node.startExpr))
        table.insert(code, ",")
        table.insert(code, self:parse(node.endExpr))

        if node.stepExpr then
            table.insert(code, ",")
            table.insert(code, self:parse(node.stepExpr))
        end

        table.insert(code, "do")
        table.insert(code, self:parseMultiple(node.block))
        table.insert(code, "end")

        return code
    end,

    ["RepeatStatNode"]
    = {"repeat", "{block}", "until", "{untilCondition}"},

    ["FunctionDeclarationStatNode"]
    = {"function", "{name}", "(", "{parameters,}", ")", "{block}", "end"},

    ["LocalFunctionDeclarationStatNode"]
    = {"local", "function", "{name}", "(", "{parameters,}", ")", "{block}", "end"},

    ["VariableAssignmentStatNode"]
    = {"{names,}", "=", "{expressions,}"},

    ["LocalVariableAssignmentStatNode"]
    = function(self, node)
        -- "local"
        local code = {"local"}

        table.insert(code, self:parseMultiple(node.names, ","))

        -- "=" "{expressions,}"
        if next(node.expressions) then
            table.insert(code, "=")
            table.insert(code, self:parseMultiple(node.expressions, ","))
        end

        return code
    end,

    ["ReturnStatNode"]
    = {"return", "{expressions,}"},

    ["BreakStatNode"]
    = {"break"},

    ["FunctionCallStatNode"]
    = Retrans.parseFunctionCallNode,

    ["GotoLabelStatNode"]
    = {"::", "{name}", "::"},

    ["GotoStatNode"]
    = {"goto", "{name}"},

    ["NumberLiteralExpNode"]
    = function(self, node)
        return {tostring(node.value)}
    end,

    ["StringLiteralExpNode"]
    = function(self, node)
        local escaped = node.value
            :gsub("\\", "\\\\")
            :gsub('"', '\\"')
            :gsub("\n", "\\n")
            :gsub("\r", "\\r")
            :gsub("\v", "\\v")
            :gsub("\t", "\\t")
            :gsub("\a", "\\a")
            :gsub("\b", "\\b")
            :gsub("\f", "\\f")

            -- Return as a single token
        return {'"' .. escaped .. '"'}
    end,

    ["NilLiteralExpNode"]
    = {"nil"},

    ["BooleanLiteralExpNode"]
    = function(self, node)
        return {node.value and "true" or "false"}
    end,

    ["TableConstructorExpNode"]
    = function(self, node)
        local code = {"{"}

        for _, v in ipairs(node.fields) do
            table.insert(code, "[")
            table.insert(code, self:parse(v.key))
            table.insert(code, "]")
            table.insert(code, "=")
            table.insert(code, self:parse(v.value))
            table.insert(code, ",")
        end

        table.insert(code, "}")

        return code
    end,

    ["FunctionCallExpNode"]
    = Retrans.parseFunctionCallNode,

    ["BinaryOpExpNode"]
    = {"{left}", "{operator}", "{right}"},

    ["UnaryOpExpNode"]
    = {"{operator}", "{expression}"},

    ["NameExpNode"]
    = {"{name}"},

    ["TableIndexExpNode"]
    = {"{target}", "[", "{key}", "]"},

    ["FunctionDefExpNode"]
    = {"function", "(", "{parameters,}", ")", "{block}", "end"},

    ["ParenthesesExpNode"]
    = {"(", "{expression}", ")"},
}


function Retrans:parseKey(node, str)
    local code = {}
    local key, comma = str:match("{(.-)(,?)}")

    if key then
        local elem = node[key]

        if type(elem) == "table" and elem.type then
            table.insert(code, self:parse(elem))
        elseif type(elem) == "table" then
            table.insert(code, self:parseMultiple(elem, comma))
        elseif type(elem) == "string" then
            table.insert(code, elem)
        else
            error("Invalid element type " .. type(elem) .. " for key '" .. key .. "'")
        end
    else
        table.insert(code, str)
    end

    return code
end


function Retrans:parseMultiple(nodes, seperator)
    local code = {}

    for k, v in ipairs(nodes) do
        if type(v) == "string" then
            table.insert(code, v)
        else
            table.insert(code, self:parse(v))
        end

        if nodes[k+1] and seperator ~= "" then
            table.insert(code, seperator)
        end
    end

    return code
end

--- Recursively re-translate the Node to a table of Lua code snippets.
---@param node Node
---@return table
function Retrans:parse(node)
    local code = {}

    local case = self.nodeSwitches[node.type]
    local caseType = type(case)

    if caseType == "function" then
        code = case(self, node)
    elseif caseType == "table" then
        for _, v in ipairs(case) do
            if type(v) == "string" then
                table.insert(code, self:parseKey(node, v))
            else
                table.insert(code, self:parse(v))
            end
        end
    elseif caseType == "nil" then
        error("No case for node " .. tostring(node))
    else
        error("Invalid case type " .. caseType .. " for node " .. node)
    end

    return code
end


--- After-process the nested tokens to eliminate lexical ambiguities.
---@param code table
function Retrans:afterprocess(code)
    -- replace each chunk's starting "(" with ";("
    for _, node in ipairs(code) do
        while node[1] do
            if node[1] == "(" then
                node[1] = ";("
            end
            node = node[1]
        end
    end
end


local function flattenInto(tbl, tbl2)
    for _, v in ipairs(tbl2) do
        if type(v) == "table" then
            flattenInto(tbl, v)
        else
            table.insert(tbl, v)
        end
    end
end

--- Re-translate the AST to Lua code.
---@param ast AbstractSyntaxTree
---@return string
function Retrans:retranslate(ast)
    local code = {}

    -- Parse individual nodes
    for _, node in ipairs(ast) do
        table.insert(code, self:parse(node))
    end

    -- Afterprocess
    self:afterprocess(code)

    -- Flatten the nested tables
    local flattened = {}
    flattenInto(flattened, code)

    -- Return the code as a string
    return table.concat(flattened, " ")
end

return Retrans