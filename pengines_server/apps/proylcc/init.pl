:- module(init, [ init/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% La aplicacion soporta grillas rectangulares de tamaños arbitrarios. NO tienen que necesariamente ser cuadrados.
% Se debe cambiar el valor de la variable de init manualmente, esto es,
% se debe hardcodear la grilla. Se opto por esta implementacion por
% sobre alguna mas extensible por el enfasis que se hizo en que 
% el levantar la grilla desde el modulo init.pl funcione correctamentet
% incluso si se implementaban variaciones en tamaño o metodo de generacion
% 
% Si bien no hay limites establecidos a nivel codigo, recomendamos encarecidamente mantenerse en el rango [3x3, 20x20]

init([
		 [y,g,b,g,v,y,p,v,b,p,v,p,v,r],
		 [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
		 [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
		 [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
		 [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
		 [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
		 [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
		 [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
		 [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
		 [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
		 [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
		 [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
		 [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
		 [v,g,p,b,v,v,g,g,g,b,v,g,g,g]
		 ]).

/* Si se des-comentan las siguientes lineas, y se comenta el init anterior se iniciara la app con una matrix de 20x20
 init([[y, p, y, p, p, v, b, g, p, v, y, v, v, v, p, g, y, r, r, g],
       [g, v, p, b, b, b, y, r, y, r, r, g, y, p, p, r, g, g, v, y],
		[p, b, v, g, p, v, b, v, v, v, p, r, b, b, y, b, g, g, r, y], 
		[y, v, y, r, p, r, b, g, p, y, y, v, g, r, y, v, b, y, p, b], 
		[b, v, p, g, v, p, y, g, p, b, r, r, b, b, y, b, v, b, p, y], 
		[p, b, b, r, r, g, r, b, g, g, r, p, v, y, y, p, g, v, v, r], 
		[r, v, g, b, b, y, y, p, p, b, b, y, y, v, g, p, b, v, r, r], 
		[v, b, b, v, g, p, b, y, r, r, r, r, y, b, p, b, v, p, b, b], 
		[p, g, r, g, y, p, y, p, r, b, r, b, b, g, b, b, y, g, r, b],
		[v, r, y, g, v, b, y, p, r, r, r, v, y, p, v, r, p, r, p, g],
		[v, g, y, p, y, v, v, b, v, r, y, v, v, b, v, p, b, y, g, r], 
		[y, y, p, r, p, b, v, r, y, r, p, y, v, v, b, g, y, r, b, y], 
		[y, g, g, b, v, g, b, y, y, g, r, b, v, y, p, r, v, y, p, b], 
		[y, p, b, b, r, p, b, g, y, y, v, p, g, y, g, p, v, r, y, p], 
		[p, g, r, v, y, r, b, p, r, y, g, g, r, p, v, p, p, p, g, p], 
		[b, g, b, v, p, g, p, r, g, v, g, v, g, v, r, g, r, y, g, b], 
		[b, g, y, b, g, p, g, y, v, b, v, v, v, y, p, b, r, p, p, b], 
		[y, g, p, b, b, g, r, y, r, g, b, g, p, v, g, r, v, b, y, r], 
		[y, y, r, g, b, b, v, g, g, p, b, y, r, v, b, p, p, y, g, b], 
		[y, g, y, b, b, p, y, p, g, r, v, v, g, p, g, r, r, p, p, y]]).
*/