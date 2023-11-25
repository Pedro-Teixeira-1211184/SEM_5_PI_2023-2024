% Predicate to convert elevator data to the desired format
convert_and_export_elevator_data(BuildingCode, ElevatorList) :-
    % Define the file name for exporting data
    output_file('elevators.pl'),
    
    % Open the file for writing
    open('elevators.pl', append, File),  % Use append mode to add to an existing file
    
    % Export elevator data to the file
    export_elevator_data(BuildingCode, ElevatorList, File),
    
    % Close the file
    close(File),
    
    write('Elevator data exported to file: elevators.pl'), nl.

% Predicate to export elevator data to a file in the desired format
export_elevator_data(_BuildingCode, [], _File) :- !.
export_elevator_data(BuildingCode, [Elevator-Floors|Rest], File) :-
    % Write the elevator data to the file in the desired format
    format(File, 'elevador(~w, [~w]).~n', [BuildingCode, Elevator]),
    format(File, '  m(_, _, _, _, ~w).~n', [BuildingCode]),  % Placeholder for elevator floors
    
    % Recursively process the rest of the elevators
    export_elevator_data(BuildingCode, Rest, File).

% Predicate to fetch elevator data from the API and export elevator information
fetch_and_export_elevator_data :-
    url(http://localhost:5050/elevators),
    output_file(OutputFile),
    
    % Perform HTTP GET request to the API
    http_get(http://localhost:5050/elevators, ElevatorData, [json_object(dict)]),
    
    % Open the file for writing
    open(OutputFile, write, File),
    
    % Export elevator data to the file
    export_elevator_data_from_api(ElevatorData, File),
    
    % Close the file
    close(File),
    
    write('Elevator data exported to file: '), write(OutputFile), nl.

% Predicate to export elevator data from the API to a file in Prolog format
export_elevator_data_from_api([], _File) :- !.
export_elevator_data_from_api([Building|Rest], File) :-
    % Extract relevant information from the API data
    BuildingCode = Building.buildingCode,
    Elevators = Building.elevators,
    
    % Convert and export the elevator data to the file
    convert_and_export_elevator_data(BuildingCode, Elevators),
    
    % Recursively process the rest of the data
    export_elevator_data_from_api(Rest, File).
