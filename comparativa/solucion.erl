-module(transitor).
-export([transicion/1, start/0]).
%lo que se hace aqui es verificar que el parametro ingresado 
%sea el correspondiente y se procede a devolver al estado de ciclo (color) correespondiente que lo sucede.

%Ejercicio-1:
transicion('en-rojo')->
    ['intermitente-rojo'];

transicion('intermitente-rojo')->
    ['en-verde'];

transicion('en-verde')->
    ['intermitente-verde'];

transicion('intermitente-verde')->
    ['en-amarillo'];

transicion('en-amarillo')->
    ['intermitente-amarillo'];

transicion('intermitente-amarillo')->
    ['en-rojo'];

transicion(_) ->
    ['estado-invalido'].


start()->
    ColorActual = 'en-amarillo',
    [Resultado] = transicion(ColorActual),
    io:format("~p~n", [Resultado]).    


-module(timee).
-export([time/0, obtener_color/1]).
%Se calcula el segundo exacto.
time() ->
    TiempoActual = os:system_time(seconds),
    CicloTotal = 225, %% 90+3+120+3+6+3 = 225
    A = TiempoActual rem CicloTotal,
    obtener_color(A).

% Se verifica el color actual del semaforo.
obtener_color(A) when A < 90 -> 
    'en-rojo';
obtener_color(A) when A < 93 ->
    'intermitente-rojo';
obtener_color(A) when A < 213 -> 
    'en-verde';
obtener_color(A) when A < 216 -> 
    'intermitente-verde';
obtener_color(A) when A < 222 -> 
    'en-amarillo';
obtener_color(A) when A < 226 -> 
    'intermitente-amarillo'.
