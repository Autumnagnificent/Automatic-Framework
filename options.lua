-- This is the Example, both to show off the Ui functions, but also to show you how to use them.

-- This should go at the top of your files. It loads the module and allows you to call it's functions with Auto.functionname
-- require("Automatic")
#include Automatic.lua

function init()
	pad = AutoUI.Pad.heavy
	space = AutoUI.Pad.thin
	-- pad = 0
	-- space = 0
end

function tick(dt)

end

function draw(dt)
	UiPush()
		UiColorFilter(0.85, 0.85, 0.85, 1)
		UiImage('gfx/backdrop.png')
		UiBlur(0.5)
	UiPop()
	
	UiPush()
		UiTranslate(AutoUI.Pad.heavy, AutoUI.Pad.heavy)
		
		AutoUI.Container(320, 120, 'left top')
		AutoUI.SpreadDown(AutoUI.Pad.thin)
			pad = AutoUI.Slider(pad, 0, 128, 128/(128/6))
			space = AutoUI.Slider(space, 0, AutoUI.Pad.heavy, 2)
		AutoUI.SpreadEnd()
	UiPop()
	
	
	UiPush()
		UiTranslate(UiCenter(), UiMiddle())

		AutoUI.Container(610, 800, 'center middle', pad)
		AutoUI.SpreadDown(space)
			

		AutoUI.SpreadEnd()
	UiPop()
end