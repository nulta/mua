-- This module re-translates the AST to Lua code.

local Defs = require("compiler.definitions")

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
    ---@type { [integer]: string, type: NODETYPE }
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

    -- Add the type tag
    code.type = node.type

    return code
end


--- After-process the nested tokens to eliminate lexical ambiguities.
---@param code table
function Retrans:afterprocessNestedTokens(code)
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


--- After-process the nested tokens to prettify the code.
--- TODO: Rewrite this function.
---@param code table
function Retrans:prettifyNestedTokens(code, indent)
    indent = indent or 0
    if type(code) ~= "table" then return end

    ---@type { [NODETYPE]: true }
    local lineBreakNodes = {
        ["DoStatNode"] = true,
        ["IfStatNode"] = true,
        ["WhileStatNode"] = true,
        ["IterativeForStatNode"] = true,
        ["NumericForStatNode"] = true,
        ["RepeatStatNode"] = true,
        ["FunctionDeclarationStatNode"] = true,
        ["LocalFunctionDeclarationStatNode"] = true,
        ["VariableAssignmentStatNode"] = true,
        ["LocalVariableAssignmentStatNode"] = true,
        ["ReturnStatNode"] = true,
        ["BreakStatNode"] = true,
        ["FunctionCallStatNode"] = true,
        ["GotoLabelStatNode"] = true,
        ["GotoStatNode"] = true,
        ["TableConstructorExpNode"] = true,
        ["FunctionDefExpNode"] = true,
    }

    local lineBreakKeywords = {
        ["do"] = true,
        ["repeat"] = true,
        ["then"] = true,
        ["else"] = true,
    }

    local indented = false
    local hasKeyword = false

    for i, v in ipairs(code) do
        if lineBreakKeywords[v] then
            indented = true
            hasKeyword = true
            table.insert(code, i + 1, "\n" .. string.rep("    ", indent))
        end

        if v == "end" then
            indented = true
            hasKeyword = true
            code[i] = "\n" .. string.rep("    ", indent - 1) .. "end\n" .. string.rep("    ", indent - 1)
            -- table.insert(code, i + 2, "\n" .. string.rep("    ", indent))
        end

        if ((code.type == "FunctionDeclarationStatNode")
        or (code.type == "LocalFunctionDeclarationStatNode")
        or (code.type == "FunctionDefExpNode")) and v == ")" then
            indented = true
            hasKeyword = true
            code[i] = "\n" .. string.rep("    ", indent) .. "end\n" .. string.rep("    ", indent)
        end
    end

    if lineBreakNodes[code.type] and not hasKeyword then
        indented = true
        table.insert(code, 1, "\n" .. string.rep("    ", indent))
    end


    for _, node in ipairs(code) do
        self:prettifyNestedTokens(node, indent + (indented and 1 or 0))
    end
end


--- Minify the flattened tokens.
---@param code table
function Retrans:processPreNoSpaces(code)
    for i, v in ipairs(code) do
        local isSymbol = Defs.LuaSymbols[v]
        local nextToken = code[i+1]

        if nextToken then
            local nonAmbiguousSymbols = {
                ["("] = true,
                ["["] = true,
                ["{"] = true,
                ["}"] = true,
                [")"] = true,
                ["]"] = true,
                [";"] = true,
                [","] = true,
            }

            local nextIsSymbol = Defs.LuaSymbols[nextToken]
            local symbolNextSymbol = (isSymbol == nextIsSymbol)
            local isNonAmbiguous = nonAmbiguousSymbols[v] or nonAmbiguousSymbols[nextToken]
            local dotNextNumber = nextToken:match("^%.") and tonumber(v)

            if (symbolNextSymbol and not isNonAmbiguous) or dotNextNumber then
                code[i] = v .. " "
            end
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
---@param flags {["prettify"]: boolean?, ["noSpaces"]: boolean?} | nil
---@return string
function Retrans:retranslate(ast, flags)
    flags = flags or {}
    local code = {}

    -- Parse individual nodes
    for _, node in ipairs(ast) do
        table.insert(code, self:parse(node))
    end

    -- Afterprocessing
    self:afterprocessNestedTokens(code)
    if flags.prettify then
        self:prettifyNestedTokens(code)
    end

    -- Flatten the nested tables
    local flattened = {}
    flattenInto(flattened, code)

    -- Return the code as a string
    if flags.noSpaces then
        self:processPreNoSpaces(flattened)
        return table.concat(flattened)
    else
        return table.concat(flattened, " ")
    end
end

return Retrans