-- This module re-translates the AST to Lua code.

local Ast = require("compiler.ast")


---@class Retrans
local Retrans = {}


---@param node FunctionCallExpNode | FunctionCallStatNode
local function parseFunctionCallNode(node)
    local code = {Retrans:parse(node.target)}
    if node.method then
        table.insert(code, ":")
        table.insert(code, node.method)
    end
    table.insert(code, "(")
    table.insert(code, Retrans:parseMultiple(node.args, ","))
    table.insert(code, ")")
    return code
end


---@type table<NODETYPE, string[] | fun(Node): string[]>
Retrans.nodeSwitches = {
    ["DoStatNode"]
    = {"do", "{block}", "end"},

    ["IfStatNode"] ---@param node IfStatNode
    = function(node)
        local code = {}

        for k, ifNode in ipairs(node.ifNodes) do
            table.insert(code, (k == 1) and "if" or "elseif")
            table.insert(code, Retrans:parse(ifNode.condition))
            table.insert(code, "then")
            table.insert(code, Retrans:parseMultiple(ifNode.block))
        end

        if node.elseBlock then
            table.insert(code, "else")
            table.insert(code, Retrans:parseMultiple(node.elseBlock))
        end

        table.insert(code, "end")

        return code
    end,

    ["WhileStatNode"]
    = {"while", "{condition}", "do", "{block}", "end"},

    ["IterativeForStatNode"]
    = {"for", "{names,}", "in", "{expressions}", "do", "{block}", "end"},

    ["NumericForStatNode"]
    = {"for", "{name}", "=", "{start}", ",", "{end}", "{step}", "do", "{block}", "end"},

    ["RepeatStatNode"]
    = {"repeat", "{block}", "until", "{condition}"},

    ["FunctionDeclarationStatNode"]
    = {"function", "{name}", "(", "{parameters}", ")", "{block}", "end"},

    ["LocalFunctionDeclarationStatNode"]
    = {"local", "function", "{name}", "(", "{parameters}", ")", "{block}", "end"},

    ["VariableAssignmentStatNode"]
    = {"{names,}", "=", "{expressions,}"},

    ["LocalVariableAssignmentStatNode"]
    = {"local", "{names,}", "=", "{expressions,}"},

    ["ReturnStatNode"]
    = {"return", "{expressions,}"},

    ["BreakStatNode"]
    = {"break"},

    ["FunctionCallStatNode"]
    = parseFunctionCallNode,

    ["GotoLabelStatNode"]
    = {"::", "{label}", "::"},

    ["GotoStatNode"]
    = {"goto", "{label}"},

    ["NumberLiteralExpNode"]
    = function (node)
        return {tostring(node.value)}
    end,

    ["StringLiteralExpNode"]
    = function(node)
        local escaped = node.value:gsub("\\", "\\\\"):gsub('"', '\\"'):gsub("\n", "\\n")
        -- Return as a single token
        return {'"' .. escaped .. '"'}
    end,

    ["NilLiteralExpNode"]
    = {"nil"},

    ["BooleanLiteralExpNode"]
    = function(node)
        return {node.value and "true" or "false"}
    end,

    ["TableConstructorExpNode"]
    = function(node)
        local code = {"{"}

        for _, v in ipairs(node.fields) do
            table.insert(code, "[")
            table.insert(code, Retrans:parse(v.key))
            table.insert(code, "]")
            table.insert(code, "=")
            table.insert(code, Retrans:parse(v.value))
            table.insert(code, ",")
        end

        table.insert(code, "}")

        return code
    end,

    ["FunctionCallExpNode"]
    = parseFunctionCallNode,

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

        if nodes[k+1] then
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
        code = case(node)
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

    for _, node in ipairs(ast) do
        flattenInto(code, self:parse(node))
    end

    return table.concat(code, " ")
end

return Retrans