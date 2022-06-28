#include AutomaticArithmetic.lua

--  "animation" parameters is used for juiciness. It is meant to be animated from 0 to 1.

padding = {none = 0, thin = 4, thick = 16, heavy = 32}

function UiEasyPad(pad, multiplyer)
	pad = pad or {'heavy', 'heavy'}
    padx = padding[pad[1]]
    pady = padding[pad[2]]
    multiplyer = multiplyer or 1
    
    UiTranslate(padx * multiplyer, pady * multiplyer)
end

function UiEasyContainer(width, height, align, _padding, windowname, animation)
    width = width or 300
    height = height or 400
    local p = _padding and padding[_padding] or padding.heavy
    align = align or 'left top'
    animation = animation and Logistic(animation, 1, -10, 0.5) or 1

    height = height * (animation + (20/height) * (1-animation))
    
    UiAlign(align)
    UiColor(0, 0, 0, 0.55 * math.min(animation * 4, 1))
    UiImageBox("ui/common/box-solid-10.png", width, height, 10, 10)
    UiWindow(width, height, true)
    hover = UiIsMouseInRect(width, height)
    
    if windowname then
        UiPush()
            if string.find(align, 'left') then
                UiAlign('left bottom')
                UiTranslate(padding.thin, -padding.thin)
            elseif string.find(align, 'center') then
                UiAlign('center bottom')
                UiTranslate(UiCenter(), -padding.thin)
            elseif string.find(align, 'right') then
                UiAlign('right bottom')
                UiTranslate(UiWidth() - padding.thin, -padding.thin)
            end
            
            UiColor(1, 1, 1, 0.75 * animation)
            UiFont("regular.ttf", 24)
            UiWordWrap(width - padding.thin)
            UiText(windowname)
        UiPop()
    end
    
    UiAlign('center top')
    UiTranslate(width / 2, p)


    return {rect = {w = width, h = height}, hover = hover}
end

function UiEasyButton(name, pad, animation)
    animation = animation and Logistic(animation, 1, -10, 0.5) or 1
    pad = pad or {'heavy', 'heavy'}
    padx = padding[pad[1]]
    pady = padding[pad[2]]

	UiPush()
        UiPush()
            UiWordWrap(UiWidth() - padding.thick)
            UiFont("regular.ttf", 28)
            
            UiColor(0,0,0,0)
            local frw, frh = UiText(name)
            UiTranslate(0,frh/2)
            UiAlign('center middle')
            
            UiColor(1,1,1,animation)
            local rw, rh = UiText(name)
            local size = math.max(rw + padding.thick, UiWidth() - padx)

            UiButtonImageBox("ui/common/box-outline-6.png", 6, 6)
            bool = UiBlankButton(size, rh + padding.thick)
            hover = UiIsMouseInRect(size, rh + padding.thick)
        UiPop()
	UiPop()

    UiAlign('center top')
    UiTranslate(0, rh + pady)

	return {value = bool, hover = hover, rect = {w = rw, h = rh}}
end

function UiEasyText(name, pad, animation)
    animation = animation and Logistic(animation, 1, -10, 0.5) or 1
    pad = pad or {'heavy', 'heavy'}
    padx = padding[pad[1]]
    pady = padding[pad[2]]
    
	UiPush()
        UiWordWrap(UiWidth() - padx)
        UiFont("regular.ttf", 28)
        
        UiPush()
            UiColor(0,0,0,0)
            local frw, frh = UiText(name)
            UiTranslate(0,frh/2)

            UiAlign('center middle')
            UiColor(1,1,1,animation)
            local rw, rh = UiText(name)
            hover = UiIsMouseInRect(rw + padding.thick, rh + padding.thick)
        UiPop()
	UiPop()

    UiAlign('center top')
    UiTranslate(0, rh + pady)

	return {value = bool, hover = hover, rect = {w = rw, h = rh}}
end

function UiGetSliderDot()
	local width, height = UiGetImageSize("ui/common/dot.png")
	return { path = "ui/common/dot.png", rect = {w = width, h = height}}
end

function UiEasySlider(pathorvalue, name, default, min, max, lockincrement)
	min = min or 0
	max = max or 1
	lockincrement = lockincrement or 0
	width = UiWidth() - UiSpacing().b * 2

	local value = nil
	if pathorvalue == nil or type(pathorvalue) == "number" then
		if pathorvalue ~= nil then
			value = pathorvalue
		else
			value = default
		end
	else
		value = GetKeyWithDefault("float", pathorvalue, default)
	end

	value = Round(value, lockincrement) * width / max - (min * width / max)

	local elapsedheight = 0

	UiPush()
		UiColor(1,1,1)
		UiWordWrap(width)
		UiFont("regular.ttf", 26)
		UiAlign('left top')
		
		UiTranslate(UiSpacing().b, 0)
		
		UiPush()
			UiTranslate(width / 2, 0)
			UiAlign('center top')
			local rw, rh = UiText(name)
			hover = UiIsMouseInRect(rw, rh)
			elapsedheight = elapsedheight + rh
		UiPop()
		
		UiColor(1,1,0.5)
		UiTranslate(0, rh + UiSpacing().f)
		elapsedheight = elapsedheight + UiSpacing().f

		UiPush()
			UiTranslate(0, -1)
			UiRect(width, 2)
			UiTranslate(0, 1)
			UiTranslate(0, -UiGetSliderDot().rect.h / 2)
			hoverslider = UiIsMouseInRect(width, UiGetSliderDot().rect.h)
		UiPop()

		UiTranslate(-UiGetSliderDot().rect.w / 2, -UiGetSliderDot().rect.h / 2)
		elapsedheight = elapsedheight + UiGetSliderDot().rect.h / 2

		value = UiSlider("ui/common/dot.png", "x", value, 0, width)

		value = (value / width) * max + min
		value = Round(value, lockincrement)
		value = math.max(value, min)
		value = math.min(value, max)

		if type(pathorvalue) == "string" then
			SetFloat(pathorvalue, value)
		end
	UiPop()

	return {value = value, hover = hover, slider = hoverslider, rect = {w = rw, h = elapsedheight}}
end

function UiEasyTrueFalse (pathorvalue, name, default, enabletext, disabletext)
	enabletext = enabletext or "Enabled"
	disabletext = disabletext or "Disabled"

	local value = nil
	if pathorvalue == nil or type(pathorvalue) ~= "string" then
		if pathorvalue ~= nil then
			value = pathorvalue
		else
			value = default
		end

		value = value
	else
		value = GetKeyWithDefault("bool", pathorvalue, default)
	end

	local text = value and enabletext or disabletext
	
	UiPush()
		local width = UiWidth() - UiSpacing().b * 2
		UiWordWrap(width / 2)
		UiFont("regular.ttf", 24)
		
		UiPush()
			UiAlign('left top')
			UiTranslate(UiSpacing().b, 0)
			UiColor(1,1,1)
			local rw, rh = UiText(name)
			
			hover = UiIsMouseInRect(rw, rh)
		UiPop()
			
		UiPush()
			UiAlign('center top')
			UiTranslate(width, 0 )
			UiColor(1,1,0.5)
			
			
			local srw, srh = UiText(text)
			hoveroption = UiIsMouseInRect(srw, srh)
			
			if hover then UiColor(1,1,0.65) UiText(text) end
			if UiBlankButton(srw, srh) then value = not value end
		UiPop()
	UiPop()

	if type(pathorvalue) == "string" then
		SetBool(pathorvalue, value)
	end

	return {value = value, hover = hover, hoveroption = hoveroption, rect = {w = rw, h = rh}}
end

function UiEasyTooltip(text, position, scale, alpha)
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

function ImageSize(path)
	UiPush()
		UiColor(0,0,0,0)
		local rectw, recth = UiImage(path)
	UiPop()

	return {w = rectw, h = recth}
end