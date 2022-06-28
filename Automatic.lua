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
function AutoUI.logistic(v, max, steep, offset)
	return max / (1 + math.exp((v - offset) * steep))
end

---'n' is the number you want rounded, 'd' is the decimal, so something like 0.1, 0.01, 1, 2, 100, 2.5
---Challenge by @TallTim and @1ssnl to make the smallest rounding function.
---@param n number
---@param d number
---@return number
function AutoUI.round(n,d)x=1/d return math.floor(n*x+.5)/x end

---Limits a value from going below the min and above the max
---@param val number
---@param min number
---@param max number
---@return number
function AutoUI.clamp(val, min, max)
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
function AutoUI.big(a, b)
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
function AutoUI.small(a, b)
	if math.abs( a ) < math.abs( b ) then
		return a
	else
		return b
	end
end

---Counts the amount of elements in a list
---@param t table
---@return integer
function AutoUI.count(t)
	local c = 0
	for i in pairs(t) do
		c = c + 1
	end

	return c
end

---Return a Random Vector
---@param length any
---@param precision any
---@return table
function AutoUI.rndVec(length, precision)
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
function AutoUI.lerpUnclamped(a, b, t)
	return (1-t)*a + t*b
end

--- Moves a towards b by t
---@param a number
---@param b number
---@param t number
---@return number
function AutoUI.move(a, b, t)
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
function AutoUI.vecDist(a, b)
	return math.sqrt( (a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2 )
end

---Return the Distance between the numbers a and b
---@param a number
---@param b number
---@return number
function AutoUI.dist(a, b)
	return math.abs(a - b)
end

-------------------------------------------------------------------------------------------------------------------------------------------------------
----------------User Interface-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

local AutoUIStack = {
	
}

local PresetPadding = {none = 0, thin = 4, thick = 16, heavy = 32, beefy = 128}
local Spacing = PresetPadding.thick
local ExitAlign = 'top' -- Can be : 'top', 'left', 'right', 'bottom'. The default is 'top'

function ToPad(pad)
	local t = type(pad)
	if t == "number" then
		return t
	elseif t == "string" then
		return PresetPadding[pad]
	elseif t == nil then
		return PresetPadding.thick
	end
end

function AutoUI.Exit()
	table.insert(AutoUIStack, {type = 'Exit'})
end

---Set the Automatic Alignment for Auto.Ui- functions. Used like UiAlign(). UiPush() and UiPop() have no effect.
---@param alignment string Can be : 'top', 'left', 'right', 'bottom'. The default is 'top'
function AutoUI.SetExitAlign(alignment)
	ExitAlign = alignment or 'top'
end

---Set the Automatic Spacing for Auto.Ui- functions. Used like padding. UiPush() and UiPop() have no effect.
---@param spacing string Can be : 'none', 'thin', 'thick', 'heavy', 'beefy'. default is 'thick'
function AutoUI.SetSpacing(spacing)
	Spacing = ToPad(spacing)
end

---UiTranslate() but for using padding values
---@param padding table Uses values from the PresetPadding, default is {"heavy", "heavy"}
---@param multiplyer number A multiplyer to the padding, default is 1
function AutoUI.Pad(padding, multiplyer)
	padding = padding or { 'thick', 'thick' }
	padding = { w = ToPad(padding[1]), h = ToPad(padding[2]) }
    multiplyer = multiplyer or 1
    
    UiTranslate(padding.w * multiplyer, padding.h * multiplyer)
end

---The Main Component of the Ui part of the Automatic Module.
---@param width number The width of the Container, default is 300
---@param height number The height of the Container, default is 400
---@param alignment string Alignment
---@param color table A table containg the red, green, blue, and alpha. default is {0, 0, 0, 0.55}
---@param name string Adds a title above the Container
---@param style string Can choose from 'solid' or 'border', default is 'solid'
---@param animation number An optional value used to animate the window, simply increment the value from 0 to 1.
---@return data table
function AutoUI.Container(width, height, alignment, color, name, style, animation)
    width = width or 300
	height = (height or 400)
    alignment = alignment or 'left top'
	color = color or {0, 0, 0, 0.55}
	style = style or 'solid'
	animation = animation and Auto.logistic(animation, 1, -10, 0.5) or 1

	height = height * (animation + (20 / height) * (1 - animation))
    
    UiAlign(alignment)
    UiColor(color[1], color[2], color[3], color[4] * math.min(animation * 4, 1))
	if style == 'solid' then
		UiImageBox("ui/common/box-solid-10.png", width, height, 10, 10)
	else
		UiImageBox("ui/common/box-outline-6.png", width, height, 6, 6)
	end
		
    UiWindow(width, height, true)
    hover = UiIsMouseInRect(width, height)

    if name then
        UiPush()
            if string.find(alignment, 'left') then
                UiAlign('left bottom')
                UiTranslate(PresetPadding.thin, -PresetPadding.thin)
            elseif string.find(alignment, 'center') then
                UiAlign('center bottom')
                UiTranslate(UiCenter(), -PresetPadding.thin)
            elseif string.find(alignment, 'right') then
                UiAlign('right bottom')
                UiTranslate(UiWidth() - PresetPadding.thin, -PresetPadding.thin)
            end
            
            UiColor(1, 1, 1, 0.75 * animation)
            UiFont("regular.ttf", 24)
            UiWordWrap(width - PresetPadding.thin)
            UiText(name)
        UiPop()
    end
    
	if ExitAlign == 'top' then
		UiAlign('center top')
		UiTranslate(width / 2, Spacing)
	elseif ExitAlign == 'bottom' then
		UiAlign('center bottom')
		UiTranslate(width / 2, height - Spacing)
	elseif ExitAlign == 'left' then
		UiAlign('left middle')
		UiTranslate(Spacing, height / 2)
	elseif ExitAlign == 'right' then
		UiAlign('right middle')
		UiTranslate(width - Spacing, height / 2)
	end

	

    return {rect = {w = width, h = height}, hover = hover}
end

---Creates a Button
---@param name string The displayed text inside the Button
---@param padding string The amount of padding between the text and the button border. default is {'thick', 'thick'}
---@param animation number An optional value used to animate the button, simply increment the value from 0 to 1.
---@return data table
function AutoUI.UiButton(name, padding, animation)
	animation = animation and Auto.logistic(animation, 1, -10, 0.5) or 1
	padding = padding or { 'thick', 'thick' }
	padding = { w = ToPad(padding[1]), h = ToPad(padding[2]) }

	UiPush()
        UiPush()
            UiWordWrap(UiWidth() - PresetPadding.thick)
            UiFont("regular.ttf", 28)
            
            UiColor(0,0,0,0)
			UiAlign('center middle')
            local rw, rh = UiText(name)
			local prw, prh = (rw + padding.w), (rh + padding.h)
			local hprw, hprh = (rw + padding.w/2), (rh + padding.h/2)
			if ExitAlign == 'top' then
				UiTranslate(0,hprh)
			elseif ExitAlign == 'bottom' then
				UiTranslate(0,-hprh)
			elseif ExitAlign == 'left' then
				UiTranslate(rw,-hprh)
			elseif ExitAlign == 'right' then
				UiTranslate(-rw,-hprh)
			end
            
            UiColor(1,1,1,animation)
            UiText(name)
			
			local hover = UiIsMouseInRect(prw, prh)
			if not hover then
				UiButtonImageBox("ui/common/box-outline-6.png", 6, 6)
			else
				UiButtonImageBox("ui/common/box-outline-fill-6.png", 6, 6)
			end
			
			local bool = UiBlankButton(prw, prh)
		UiPop()
	UiPop()
			
	local trw, trh = prw + 6, prh + 6
	if ExitAlign == 'top' then
		UiTranslate(0, trh + Spacing)
	elseif ExitAlign == 'bottom' then
		UiTranslate(0, -(trh + Spacing))
	elseif ExitAlign == 'left' then
		UiTranslate(trw + Spacing, 0)
	elseif ExitAlign == 'right' then
		UiTranslate(-(trw + Spacing), 0)
	end

	return {value = bool, hover = hover, rect = {w = trw, h = trh}}
end

-- function AutoUI.UiEasyText(name, pad, animation)
-- 	animation = animation and Auto.logistic(animation, 1, -10, 0.5) or 1
--     pad = pad or {'heavy', 'heavy'}
--     padx = PresetPadding[pad[1]]
--     pady = PresetPadding[pad[2]]
    
-- 	UiPush()
--         UiWordWrap(UiWidth() - padx)
--         UiFont("regular.ttf", 28)
        
--         UiPush()
--             UiColor(0,0,0,0)
--             local frw, frh = UiText(name)
--             UiTranslate(0,frh/2)

--             UiAlign('center middle')
--             UiColor(1,1,1,animation)
--             local rw, rh = UiText(name)
--             hover = UiIsMouseInRect(rw + PresetPadding.thick, rh + PresetPadding.thick)
--         UiPop()
-- 	UiPop()

--     UiAlign('center top')
--     UiTranslate(0, rh + pady)

-- 	return {value = bool, hover = hover, rect = {w = rw, h = rh}}
-- end

-- function AutoUI.UiEasySlider(pathorvalue, name, default, min, max, lockincrement)
-- 	min = min or 0
-- 	max = max or 1
-- 	lockincrement = lockincrement or 0
-- 	width = UiWidth() - UiSpacing().b * 2

-- 	local value = nil
-- 	if pathorvalue == nil or type(pathorvalue) == "number" then
-- 		if pathorvalue ~= nil then
-- 			value = pathorvalue
-- 		else
-- 			value = default
-- 		end
-- 	else
-- 		value = GetKeyWithDefault("float", pathorvalue, default)
-- 	end

-- 	value = Round(value, lockincrement) * width / max - (min * width / max)

-- 	local elapsedheight = 0

-- 	UiPush()
-- 		UiColor(1,1,1)
-- 		UiWordWrap(width)
-- 		UiFont("regular.ttf", 26)
-- 		UiAlign('left top')
		
-- 		UiTranslate(UiSpacing().b, 0)
		
-- 		UiPush()
-- 			UiTranslate(width / 2, 0)
-- 			UiAlign('center top')
-- 			local rw, rh = UiText(name)
-- 			hover = UiIsMouseInRect(rw, rh)
-- 			elapsedheight = elapsedheight + rh
-- 		UiPop()
		
-- 		UiColor(1,1,0.5)
-- 		UiTranslate(0, rh + UiSpacing().f)
-- 		elapsedheight = elapsedheight + UiSpacing().f

-- 		UiPush()
-- 			UiTranslate(0, -1)
-- 			UiRect(width, 2)
-- 			UiTranslate(0, 1)
-- 			UiTranslate(0, -UiGetSliderDot().rect.h / 2)
-- 			hoverslider = UiIsMouseInRect(width, UiGetSliderDot().rect.h)
-- 		UiPop()

-- 		UiTranslate(-UiGetSliderDot().rect.w / 2, -UiGetSliderDot().rect.h / 2)
-- 		elapsedheight = elapsedheight + UiGetSliderDot().rect.h / 2

-- 		value = UiSlider("ui/common/dot.png", "x", value, 0, width)

-- 		value = (value / width) * max + min
-- 		value = Round(value, lockincrement)
-- 		value = math.max(value, min)
-- 		value = math.min(value, max)

-- 		if type(pathorvalue) == "string" then
-- 			SetFloat(pathorvalue, value)
-- 		end
-- 	UiPop()

-- 	return {value = value, hover = hover, slider = hoverslider, rect = {w = rw, h = elapsedheight}}
-- end

-- function AutoUI.UiEasyTrueFalse (pathorvalue, name, default, enabletext, disabletext)
-- 	enabletext = enabletext or "Enabled"
-- 	disabletext = disabletext or "Disabled"

-- 	local value = nil
-- 	if pathorvalue == nil or type(pathorvalue) ~= "string" then
-- 		if pathorvalue ~= nil then
-- 			value = pathorvalue
-- 		else
-- 			value = default
-- 		end

-- 		value = value
-- 	else
-- 		value = GetKeyWithDefault("bool", pathorvalue, default)
-- 	end

-- 	local text = value and enabletext or disabletext
	
-- 	UiPush()
-- 		local width = UiWidth() - UiSpacing().b * 2
-- 		UiWordWrap(width / 2)
-- 		UiFont("regular.ttf", 24)
		
-- 		UiPush()
-- 			UiAlign('left top')
-- 			UiTranslate(UiSpacing().b, 0)
-- 			UiColor(1,1,1)
-- 			local rw, rh = UiText(name)
			
-- 			hover = UiIsMouseInRect(rw, rh)
-- 		UiPop()
			
-- 		UiPush()
-- 			UiAlign('center top')
-- 			UiTranslate(width, 0 )
-- 			UiColor(1,1,0.5)
			
			
-- 			local srw, srh = UiText(text)
-- 			hoveroption = UiIsMouseInRect(srw, srh)
			
-- 			if hover then UiColor(1,1,0.65) UiText(text) end
-- 			if UiBlankButton(srw, srh) then value = not value end
-- 		UiPop()
-- 	UiPop()

-- 	if type(pathorvalue) == "string" then
-- 		SetBool(pathorvalue, value)
-- 	end

-- 	return {value = value, hover = hover, hoveroption = hoveroption, rect = {w = rw, h = rh}}
-- end

function AutoUI.UiWorldTooltip(text, position, scale, alpha)
    text = text or "nil"
	scale = scale or 1
    alpha = alpha or 0.75

	UiPush()
        UiAlign('center middle')
        local x, y = UiWorldToPixel(position)
        UiTranslate(x, y)
        UiWordWrap(UiMiddle())

		UiScale(scale)

		UiFont("regular.ttf", 24)
		UiColor(0,0,0,0)
		local rw, rh = UiText(text)

		UiColor(0,0,0,alpha)
		UiRect(rw,rh)
		
		UiColor(1,1,1,alpha)
		UiText(text)
	UiPop()
end

function AutoUI.UiMarker()
	UiPop()
		UiColor(1, 1, 0.5)
		UiAlign('center middle')
		UiImage('ui/common/dot.png')
	UiPush()
end