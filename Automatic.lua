Auto = {}
AutoUI = {}

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Arithmetic-----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Logistic function, Can be used for juicy UI among other things.
---https://www.desmos.com/calculator/cmmwrjtyit?invertedColors
---@param v number
---@param max number
---@param steep number
---@param offset number
---@return number
function Auto.Logistic(v, max, steep, offset)
	return max / (1 + math.exp((v - offset) * steep))
end

---'n' is the number you want rounded, 'd' is the decimal, so something like 0.1, 0.01, 1, 2, 100, 2.5
---Challenge by @TallTim and @1ssnl to make the smallest rounding function.
---@param n number
---@param d number
---@return number
function Auto.Round(n,d)x=1/d return math.floor(n*x+.5)/x end

---Limits a value from going below the min and above the max
---@param val number
---@param min number
---@param max number
---@return number
function Auto.Clamp(val, min, max)
	if val < min then
		return min
	elseif val > max then
		return max
	else
		return val
	end
end

---Returns the value that is farthest from zero
---@param a number
---@param b number
---@return number
function Auto.Big(a, b)
	if math.abs( a ) > math.abs( b ) then
		return a
	else
		return b
	end
end

---Returns the value that is closest to zero
---@param a number
---@param b number
---@return number
function Auto.Small(a, b)
	if math.abs( a ) < math.abs( b ) then
		return a
	else
		return b
	end
end

---Return a Random Vector
---@param length any
---@param precision any
---@return table
function Auto.RndVec(length, precision)
	precision = precision or 0.01
	local m = 1/precision
	local v = VecNormalize(Vec(math.random(-m,m), math.random(-m,m), math.random(-m,m)))
	return VecScale(v, length)	
end

---Lerp function, Is not clamped meaning it if t is above 1 then it will 'overshoot'
---@param a number
---@param b number
---@param t number
---@return number
function Auto.LerpUnclamped(a, b, t)
	return (1-t)*a + t*b
end

--- Moves a towards b by t
---@param a number
---@param b number
---@param t number
---@return number
function Auto.Move(a, b, t)
	output = a
    if a == b then
		return a
	end

	if a > b then
		output = math.max(a - t, b)
	else
		output = math.min(a + t, b)
	end

	return output
end

---Return the Distance between Two Vectors
---@param a Vec
---@param b Vec
---@return number
function Auto.VecDist(a, b)
	return math.sqrt( (a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2 )
end

---Return the Distance between the numbers a and b
---@param a number
---@param b number
---@return number
function Auto.Dist(a, b)
	return math.abs(a - b)
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------Utility-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

---Counts the amount of elements in a list
---@param t table
---@return integer
function Auto.Count(t)
	local c = 0
	for i in pairs(t) do
		c = c + 1
	end

	return c
end

function Auto.Skip(t)
	local new = {}
	for i=1, Auto.Count(t) do
		new[i] = t[i]
	end
	return new
end

---A workaround to making a table readonly
---@param tbl table
---@return table
function Auto.SetReadOnly(tbl)
	return setmetatable({}, {
		__index = tbl,
		__newindex = function(t, key, value)
			error("attempting to change constant " ..
				tostring(key) .. " to " .. tostring(value), 2)
		end
	})
end

function Auto.ToString(...)
	local output = ""

	for i=1, Auto.Count(arg) - 1 do
		local v = arg[i]
		if output ~= "" then output = output .. " : " end
		
		local t = type(v)
		if t ~= "table" then
			output = output .. tostring(v)
		else
			local localoutput = ""
			for i, v in ipairs(v) do
				if localoutput ~= "" then localoutput = localoutput .. ", " end
				localoutput = localoutput .. Auto.ToString(v)
			end
			output = output..localoutput
		end

	end

	return output
end

function Auto.Print(...)
	local output = ""

	for index = 1, Auto.Count(arg) - 1 do
		local value = arg[index]
		if output ~= "" then output = output .. " : " end
		
		local t = type(value)
		if t == 'nil' then
			output = output .. "nil"
		elseif t ~= "table" then
			output = output .. tostring(value)
		else
			local localoutput = ""
			for i, v in ipairs(value) do
				if localoutput ~= "" then localoutput = localoutput .. ", " end
				localoutput = localoutput .. Auto.ToString(v)
			end
			output = output..localoutput
		end

	end

	DebugPrint(output)
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

local PresetPadding = Auto.SetReadOnly({none = 0, thin = 4, thick = 16, heavy = 32, beefy = 128})
local Stack = {}

local function ToPad(pad)
	local t = type(pad)
	if t == "number" then
		return t
	elseif t == "string" then
		return PresetPadding[pad]
	elseif t == nil then
		return PresetPadding.thick
	end
end

function AutoUI.Pad(padding, multiplyer)
	padding = padding or { 'thick', 'thick' }
	padding = { w = ToPad(padding[1]), h = ToPad(padding[2]) }
    multiplyer = multiplyer or 1
    
    UiTranslate(padding.w * multiplyer, padding.h * multiplyer)
end
function AutoUI.Container(width, height, align, _padding, windowname, animation)
	width = width or 300
	height = height or 400
	local p = _padding and PresetPadding[_padding] or PresetPadding.heavy
	align = align or 'left top'
	animation = animation and Auto.Logistic(animation, 1, -10, 0.5) or 1

	height = height * (animation + (20 / height) * (1 - animation))

	UiAlign(align)
	UiColor(0, 0, 0, 0.55 * math.min(animation * 4, 1))
	UiImageBox("ui/common/box-solid-10.png", width, height, 10, 10)
	UiWindow(width, height, true)
	hover = UiIsMouseInRect(width, height)

	if windowname then
		UiPush()
		if string.find(align, 'left') then
			UiAlign('left bottom')
			UiTranslate(PresetPadding.thin, -PresetPadding.thin)
		elseif string.find(align, 'center') then
			UiAlign('center bottom')
			UiTranslate(UiCenter(), -PresetPadding.thin)
		elseif string.find(align, 'right') then
			UiAlign('right bottom')
			UiTranslate(UiWidth() - PresetPadding.thin, -PresetPadding.thin)
		end

		UiColor(1, 1, 1, 0.75 * animation)
		UiFont("regular.ttf", 24)
		UiWordWrap(width - PresetPadding.thin)
		UiText(windowname)
		UiPop()
	end

	UiAlign('center top')
	UiTranslate(width / 2, p)


	return { rect = { w = width, h = height }, hover = hover }
end

function AutoUI.Button(name, pad, animation)
	animation = animation and Auto.Logistic(animation, 1, -10, 0.5) or 1
	pad = pad or { 'heavy', 'heavy' }
	padx = PresetPadding[ pad[1] ]
	pady = PresetPadding[ pad[2] ]

	UiPush()
	UiPush()
	UiWordWrap(UiWidth() - PresetPadding.thick)
	UiFont("regular.ttf", 28)

	UiColor(0, 0, 0, 0)
	local frw, frh = UiText(name)
	UiTranslate(0, frh / 2)
	UiAlign('center middle')

	UiColor(1, 1, 1, animation)
	local rw, rh = UiText(name)
	local size = math.max(rw + PresetPadding.thick, UiWidth() - padx)

	UiButtonImageBox("ui/common/box-outline-6.png", 6, 6)
	bool = UiBlankButton(size, rh + PresetPadding.thick)
	hover = UiIsMouseInRect(size, rh + PresetPadding.thick)
	UiPop()
	UiPop()

	UiAlign('center top')
	UiTranslate(0, rh + pady)

	return { value = bool, hover = hover, rect = { w = rw, h = rh } }
end

function AutoUI.Text(name, pad, animation)
	animation = animation and Auto.Logistic(animation, 1, -10, 0.5) or 1
	pad = pad or { 'heavy', 'heavy' }
	padx = PresetPadding[ pad[1] ]
	pady = PresetPadding[ pad[2] ]

	UiPush()
	UiWordWrap(UiWidth() - padx)
	UiFont("regular.ttf", 28)

	UiPush()
	UiColor(0, 0, 0, 0)
	local frw, frh = UiText(name)
	UiTranslate(0, frh / 2)

	UiAlign('center middle')
	UiColor(1, 1, 1, animation)
	local rw, rh = UiText(name)
	hover = UiIsMouseInRect(rw + PresetPadding.thick, rh + PresetPadding.thick)
	UiPop()
	UiPop()

	UiAlign('center top')
	UiTranslate(0, rh + pady)

	return { value = bool, hover = hover, rect = { w = rw, h = rh } }
end

function AutoUI.Marker(size)
	size = (size or 1) / 2
	AutoUI.Push()
		UiScale(size, size)
		UiColor(1, 1, 0.55, 1)
		UiImage('ui/common/dot.png')
	AutoUI.Pop()
end