-- This is the Example, both to show off the Ui functions, but also to show you how to use them.

-- This should go at the top of your files. It loads the module and allows you to call it's functions with Auto.functionname
-- require("Automatic")
#include Automatic.lua

function init()
	hue = 0
	sat = 1
	val = 1
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
		AutoCenter()
		AutoContainer(500, 500, AutoPad.thick)
		AutoSpreadDown(AutoPad.thick)
		
			UiPush()
				UiTranslate(UiCenter())
				UiAlign('center top')
				
				local color = AutoHSVToRGB(hue, sat, val)
				UiColor(color[1], color[2], color[3], 1)
				UiRect(250, 250)
				
				UiAlign('center middle')
				UiTranslate(0, 250/2)
				local color = AutoHSVToRGB(hue, sat, 1)
				UiColor(color[1], color[2], color[3], 1)
				UiRect(100, 100)

				UiAlign('center middle')
				UiTranslate(250/2 - 25/2)
				UiColor(1, 1, 1, 1)
				UiRect(25, 100)
			UiPop()

			UiAlign('left top')
			UiTranslate(0, 250 + AutoPad.heavy)
			
			sat = AutoSlider(sat)
			val = AutoSlider(val)
			
		AutoSpreadEnd()
	UiPop()
end