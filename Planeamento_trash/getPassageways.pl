:- module(getPassageways, [fetch_and_export_passageway_data/0]).

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Define the output file for passageway data
output_file_passageways('passageways.pl').

% Predicate to convert passageway data to the desired format
convert_and_export_passageway_data(FloorCode1, FloorCode2) :-
    % Open the file for writing
    output_file_passageways(OutputFile),
    open(OutputFile, append, File),  % Use append mode to add to an existing file
    
    % Export passageway data to the file
    format(File, 'corredor(~w, ~w).~n', [FloorCode1, FloorCode2]),
    
    % Close the file
    close(File),
    
    write('Passageway data exported to file: passageways.pl'), nl.

% Predicate to fetch passageway data from the API and export passageway information
fetch_and_export_passageway_data :-
    % Open the URL and get the content
    http_open('http://localhost:5050/passageways', In, []),
    json_read_dict(In, PassagewayData),
    close(In),
    
    % Export passageway data to the file
    export_passageway_data_from_api(PassagewayData).

% Predicate to export passageway data from the API to a file in Prolog format
export_passageway_data_from_api([], _File) :- !.
export_passageway_data_from_api([Passageway|Rest]) :-
    % Extract relevant information from the API data
    FloorCode1 = Passageway.get(floorCode1),
    FloorCode2 = Passageway.get(floorCode2),
    
    % Convert and export the passageway data to the file
    convert_and_export_passageway_data(FloorCode1, FloorCode2),
    
    % Recursively process the rest of the data
    export_passageway_data_from_api(Rest).

% Example usage:
% Call fetch_and_export_passageway_data/0 to fetch data from the API and export it to the file
