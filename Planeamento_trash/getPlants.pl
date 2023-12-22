:- module(getPlants, [export_plant_map_data_from_api/2]).

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Define the output file for plant map data
output_file_plants('plants_map.pl').

% Predicate to convert plant map data to the desired format
convert_and_export_plant_map(Map) :-
    % Get the output file name
    output_file_plants(OutputFile),
    
    % Open the file for writing
    open(OutputFile, append, File),  % Use append mode to add to an existing file
    
    % Export plant map data to the file
    map_export(Map, 1, File),
    
    % Close the file
    close(File),
    
    write('Plant map data exported to file: plants_map.pl'), nl.

% Predicate to export only the plant map data to a file in Prolog format
map_export([], _, _) :- !.
map_export([Plant|Rest], FloorCode, File) :-
    Width = Plant.width,
    Length = Plant.length,
    Map = Plant.map,
    
    % Write the plant map data to the file
    format(File, 'floorCode(~w, ~w, ~w, [', [FloorCode, Width, Length]),
    export_plant_map_data(Map, File),
    
    % Close the parenthesis
    format(File, ']).~n', []),
    
    % Recursively process the rest of the plant data
    FloorCodeNext is FloorCode + 1,
    map_export(Rest, FloorCodeNext, File).

% Predicate to export each row of plant map data to the file
export_plant_map_data([], _File) :- !.
export_plant_map_data([Row|Rest], File) :-
    export_plant_map_line_cols(1, Row, File),
    
    % Recursively process the rest of the rows
    export_plant_map_data(Rest, File).

% Predicate to export each column of a row of plant map data to the file
export_plant_map_line_cols(_, [], _File) :- !.
export_plant_map_line_cols(Col, [Value|Rest], File) :-
    % Write the plant map data to the file in the desired format
    format(File, '~w', [Value]),
    
    % Add a comma when its not the last element
    (Rest = [] -> format(File, '', []); format(File, ', ', [])),
    
    % Recursively process the rest of the columns
    Col1 is Col + 1,
    export_plant_map_line_cols(Col1, Rest, File).

% Predicate to fetch plant map data from the API and export plant map information
fetch_and_export_plant_map_data :-
    % Specify the API endpoint
    url('http://localhost:5050/maps/allPlants'),

    % Perform HTTP GET request to the API
    http_get('http://localhost:5050/maps/allPlants', PlantMapData, [json_object(dict)]),

    % Export plant map data to the file
    export_plant_map_data_from_api(PlantMapData).

% Predicate to export plant map data from the API to a file in Prolog format
export_plant_map_data_from_api([], _File) :- !.
export_plant_map_data_from_api(PlantMapData) :-
    % Convert and export the plant map data to the file
    convert_and_export_plant_map(PlantMapData).
