% Predicate to convert passageway data to the desired format
convert_and_export_passageway_data(BuildingCodeFrom, BuildingCodeTo, Floors) :-
    % Define the file name for exporting data
    output_file('passageways.pl'),
    
    % Open the file for writing
    open('passageways.pl', append, File),  % Use append mode to add to an existing file
    
    % Export passageway data to the file
    export_passageway_data(BuildingCodeFrom, BuildingCodeTo, Floors, File),
    
    % Close the file
    close(File),
    
    write('Passageway data exported to file: passageways.pl'), nl.

% Predicate to export passageway data to a file in the desired format
export_passageway_data(BuildingCodeFrom, BuildingCodeTo, [], File) :- !.
export_passageway_data(BuildingCodeFrom, BuildingCodeTo, [Floor|Rest], File) :-
    % Write the passageway data to the file in the desired format
    format(File, 'corredor(~w, ~w, ~w, ~w).~n', [BuildingCodeFrom, BuildingCodeTo, BuildingCodeFrom, BuildingCodeTo]),
    
    % Recursively process the rest of the floors
    export_passageway_data(BuildingCodeFrom, BuildingCodeTo, Rest, File).

% Predicate to fetch passageway data from the API and export passageway information
fetch_and_export_passageway_data :-
    url(http://localhost:5050/passageways),
    output_file(OutputFile),
    
    % Perform HTTP GET request to the API
    http_get(http://localhost:5050/passageways, PassagewayData, [json_object(dict)]),
    
    % Open the file for writing
    open(OutputFile, write, File),
    
    % Export passageway data to the file
    export_passageway_data_from_api(PassagewayData, File),
    
    % Close the file
    close(File),
    
    write('Passageway data exported to file: '), write(OutputFile), nl.

% Predicate to export passageway data from the API to a file in Prolog format
export_passageway_data_from_api([], _File) :- !.
export_passageway_data_from_api([Passageway|Rest], File) :-
    % Extract relevant information from the API data
    BuildingCodeFrom = Passageway.buildingCodeFrom,
    BuildingCodeTo = Passageway.buildingCodeTo,
    Floors = Passageway.floors,
    
    % Convert and export the passageway data to the file
    convert_and_export_passageway_data(BuildingCodeFrom, BuildingCodeTo, Floors),
    
    % Recursively process the rest of the data
    export_passageway_data_from_api(Rest, File).
