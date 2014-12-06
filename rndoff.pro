; 9-2-97

function rndoff, x

y = long(x)
return, y+long((x-y)*2.)

end
