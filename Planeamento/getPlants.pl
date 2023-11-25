% Predicate to convert plant map data to the desired format
convert_and_export_plant_map(FloorCode, Width, Length, Map) :-
    % Define the file name for exporting data
    output_file('plants_map.pl'),
    
    % Open the file for writing
    open('plants_map.pl', append, File),  % Use append mode to add to an existing file
    
    % Export plant map data to the file
    export_plant_map_data(FloorCode, Width, Length, Map, File),
    
    % Close the file
    close(File),
    
    write('Plant map data exported to file: plants_map.pl'), nl.

% Predicate to export plant map data to a file in the desired format
export_plant_map_data(_FloorCode, _Width, 0, _Map, _File) :- !.
export_plant_map_data(FloorCode, Width, Lin, Map, File) :-
    export_plant_map_line(FloorCode, Width, Lin, Map, File),
    Lin1 is Lin - 1,
    export_plant_map_data(FloorCode, Width, Lin1, Map, File).

% Predicate to export each line of plant map data to the file
export_plant_map_line(FloorCode, Width, Lin, Map, File) :-
    nth1(Lin, Map, Line),
    export_plant_map_line_cols(FloorCode, 1, Width, Lin, Line, File).

% Predicate to export each column of a line of plant map data to the file
export_plant_map_line_cols(_FloorCode, Col, _Width, _Lin, [], _File) :- !.
export_plant_map_line_cols(FloorCode, Col, Width, Lin, [Value|Rest], File) :-
    % Define the type based on the value
    (Value =:= 1 -> Tipo = 1; Value =:= 2 -> Tipo = 2; Value =:= 3 -> Tipo = 3; Value =:= 4 -> Tipo = 4; Tipo = 0),
    
    % Write the plant map data to the file in the desired format
    format(File, 'm(~w, ~w, ~w, ~w).~n', [Col, Lin, Value, Tipo]),
    
    % Recursively process the rest of the columns
    Col1 is Col + 1,
    export_plant_map_line_cols(FloorCode, Col1, Width, Lin, Rest, File).

% Predicate to fetch plant data from the API and export only the map information
fetch_and_export_plant_map :-
    url(http://localhost:5050/maps),
    output_file(OutputFile),
    
    % Perform HTTP GET request to the API
    http_get(http://localhost:5050/maps, PlantsData, [json_object(dict)]),
    
    % Open the file for writing
    open(OutputFile, write, File),
    
    % Export only the plant map data to the file
    export_plant_map_data_from_plants_data(PlantsData, File),
    
    % Close the file
    close(File),
    
    write('Plant map data exported to file: '), write(OutputFile), nl.

% Predicate to export only the plant map data to a file in Prolog format
export_plant_map_data_from_plants_data([], _File) :- !.
export_plant_map_data_from_plants_data([Plant|Rest], File) :-
    % Extract relevant information from the plant data
    FloorCode = Plant.floorCode,
    Width = Plant.width,
    Length = Plant.length,
    Map = Plant.map,
    
    % Convert and export the plant map data to the file
    convert_and_export_plant_map(FloorCode, Width, Length, Map),
    
    % Recursively process the rest of the plant data
    export_plant_map_data_from_plants_data(Rest, File).
