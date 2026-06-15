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

% Función puente para que Sublime muestre el resultado directo
start()->
    ColorActual = 'en-amarillo',
    [Resultado] = transicion(ColorActual),
    io:format("~p~n", [Resultado]).    
