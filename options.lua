-- This is the Example, both to show off the Ui functions, but also to show you how to use them.

-- This should go at the top of your files. It loads the module and allows you to call it's functions with Auto.functionname
-- require("Automatic")
#include Automatic.lua

function init()
	pad = AutoUIPad.heavy
	space = AutoUIPad.thin
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
		UiTranslate(AutoUIPad.heavy, AutoUIPad.heavy)
		
		AutoUIContainer(320, 120, 'left top')
		AutoUISpreadDown(AutoUIPad.thin)
			pad = AutoUISlider(pad, 0, 128, 128/(128/6))
			space = AutoUISlider(space, 0, AutoUIPad.heavy, 2)
		AutoUISpreadEnd()
	UiPop()
	
	
	UiPush()
		UiTranslate(UiCenter(), UiMiddle())

		AutoUIContainer(610, 800, 'center middle', pad)
		AutoUIMarker()
		AutoUISpreadDown(space)
			

		AutoUISpreadEnd()
	UiPop()
end