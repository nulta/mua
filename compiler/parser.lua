
---@class Parser
---@field tokens TokenSequence
local Parser = {}

function Parser.new()
    local self = setmetatable({}, { __index = Parser })
    return self
end