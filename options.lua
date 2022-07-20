-- This is the Example, both to show off the Ui functions, but also to show you how to use them.

-- This should go at the top of your files. It loads the module and allows you to call it's functions with Auto.functionname
-- require("Automatic")
#include Automatic.lua

function init()

end

function tick(dt)

end

function draw(dt)
	UiPush()
		UiColorFilter(0.85, 0.85, 0.85, 1)
		UiImage('gfx/backdrop.png')
		UiBlur(0.5)
	UiPop()
end