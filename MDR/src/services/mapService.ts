import {Service, Inject} from 'typedi';
import config from "../../config";
import IMapService from "./IServices/IMapService";
import {Result} from "../core/logic/Result";
import IMapDTO from "../dto/IMapDTO";
import IMapRepo from './IRepos/IMapRepo';
import {Map} from "../domain/map";
import {MapMapper} from "../mappers/MapMapper";
import IFloorRepo from "./IRepos/IFloorRepo";
import IRoomRepo from "./IRepos/IRoomRepo";
import IPassagewayRepo from "./IRepos/IPassagewayRepo";
import IElevatorRepo from "./IRepos/IElevatorRepo";
import IBuildingRepo from "./IRepos/IBuildingRepo";
import IPlantDTO from '../dto/IPlantDTO';
//import { http } from 'winston';
import IPathDTO from '../dto/IPathDTO';
import IPathResultDTO from '../dto/IPathResultDTO';
const http = require('http');


@Service()

export default class MapService implements IMapService {

    constructor(
        @Inject(config.repos.map.name) private mapRepo: IMapRepo,
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
        @Inject(config.repos.passageway.name) private passagewayRepo: IPassagewayRepo,
        @Inject(config.repos.elevator.name) private elevatorRepo: IElevatorRepo
    ) {
    }

    public async createMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
        try {
            //get building
            const building = await this.buildingRepo.findByCode(mapDTO.buildingCode);
            if (building == null) {
                return Result.fail<IMapDTO>("Building does not exist.");
            }

            //check size
            const length = parseInt(building.dimensions.length.toString());
            const width = parseInt(building.dimensions.width.toString());
            if (mapDTO.size.width - 1 != width || mapDTO.size.length - 1 != length) {
                return Result.fail<IMapDTO>("Map size does not match building size.");
            }
            //verify if floor exists
            const floorExists = await this.floorRepo.existsByBCodeAndNumber(mapDTO.buildingCode, mapDTO.floorNumber);
            if (floorExists) {
                return Result.fail<IMapDTO>("Floor does not exist.");
            }

            //verify if rooms exist
            if (mapDTO.rooms != undefined) {
                for (let room of mapDTO.rooms) {
                    const roomExists = await this.roomRepo.findByName(room.name);
                    if (roomExists == null) {
                        return Result.fail<IMapDTO>("Room " + room.name + " does not exist.");
                    }
                    const test = await this.floorRepo.findByDomainId(roomExists.floorCode);
                    if (test == null || test.buildingCode != mapDTO.buildingCode || test.number != mapDTO.floorNumber) {
                        return Result.fail<IMapDTO>("Room " + room.name + " does not belong to this floor.");
                    }
                }
            }

            //verify if passageways exist
            if (mapDTO.passageways != undefined) {
                for (let passageway of mapDTO.passageways) {
                    const passagewayExists = await this.passagewayRepo.findByFloorCodes(passageway.start, passageway.end);
                    if (passagewayExists == null) {
                        return Result.fail<IMapDTO>("Passageway " + passageway.start + "-" + passageway.end + " does not exist.");
                    }
                }
            }

            console.log(mapDTO.elevator);

            if (mapDTO.elevator != undefined) {
                //verify if elevator exists
                const elevatorExists = await this.elevatorRepo.findByBuildingCode(mapDTO.buildingCode);
                //get floors where elevator exists
                const floorsWithElevator = elevatorExists.floorNumbers.split(",");
                //check if elevator exists in this floor
                if (!floorsWithElevator.includes(mapDTO.floorNumber.toString())) {
                    return Result.fail<IMapDTO>("Elevator does not exist in this floor.");
                }
            }

            const mapOrError = Map.create(mapDTO);
            if (mapOrError.isFailure) {
                return Result.fail<IMapDTO>(mapOrError.errorValue());
            }
            const map = mapOrError.getValue();
            const mapExists = await this.mapRepo.exists(map);
            if (!mapExists) {
                return Result.fail<IMapDTO>("Map already exists.");
            }
            const mapDocument = await this.mapRepo.save(map);
            mapDTO = MapMapper.toDTO(mapDocument);

            return Result.ok<IMapDTO>(mapDTO);
        } catch (e) {
            throw e;
        }
    }


    
    public async loadMap(buildingCode: string, floorNumber: number): Promise<Result<IMapDTO>> {
        try {
            const mapOrError = await this.mapRepo.findByBuildingCodeAndFloorNumber(buildingCode, floorNumber);
            const mapExists = await this.mapRepo.exists(MapMapper.toDomain(mapOrError));
            if (!mapExists) {
                return Result.fail<IMapDTO>("Map does not exist.");
            }
            return Result.ok<IMapDTO>(mapOrError);
        } catch (e) {
            throw e;
        }
    }
    
    public async listMaps(): Promise<Result<IPlantDTO[]>> {
        try{
            const mapOrError = await this.mapRepo.findAll();
            if (mapOrError.length == 0) {
                return Result.fail<IPlantDTO[]>("No maps found.");
            }
            let plants: IPlantDTO[] = [];
            for (let i = 0; i < mapOrError.length; i++) {
                const mapDTO = await MapMapper.toDTO(mapOrError[i]);
                const plantDTO = await this.turnToPlant(mapDTO);
                plants.push(plantDTO);
            }
            return Result.ok<IPlantDTO[]>(plants);
        }catch(e){
            throw e;
        }
    }
    


    public async turnToPlant(mapDTO: IMapDTO): Promise<IPlantDTO> {
        try{
            const plantDTO: IPlantDTO = {
                floorCode: `${mapDTO.buildingCode}-${mapDTO.floorNumber}`,
                width: mapDTO.size.width,
                length: mapDTO.size.length,
                map: mapDTO.map.map((row) => [...row]), // Create a deep copy of the original map
                passageways: mapDTO.map.map(() => Array(mapDTO.size.width).fill(0)),
                elevator: mapDTO.map.map(() => Array(mapDTO.size.width).fill(0)),
                rooms: mapDTO.map.map(() => Array(mapDTO.size.width).fill(0)),
              };
            
              // Update the map with room information
              mapDTO.rooms.forEach((room) => {
                for (let x = room.dimensions.top.x; x <= room.dimensions.bottom.x; x++) {
                  for (let y = room.dimensions.top.y; y <= room.dimensions.bottom.y; y++) {
                    plantDTO.map[y][x] = 5; // Walls
                  }
                }
                const doorX = room.door.coordinates.x;
                const doorY = room.door.coordinates.y;
                plantDTO.map[doorY][doorX] = 3; // Door
                plantDTO.rooms[doorY][doorX] = 3; // Room identifier
              });
            
              // Update the map with passageway information
              mapDTO.passageways.forEach((passageway) => {
                const x = passageway.localization.coordinates.x;
                const y = passageway.localization.coordinates.y;
                plantDTO.map[y][x] = 4; // Passageway
                plantDTO.passageways[y][x] = 4; // Passageway identifier
              });
            
              // Update the map with elevator information
              mapDTO.elevator.forEach((elevator) => {
                const x = elevator.localization.coordinates.x;
                const y = elevator.localization.coordinates.y;
                plantDTO.map[y][x] = 2; // Elevator
                plantDTO.elevator[y][x] = 2; // Elevator identifier
              });
            
              return plantDTO;
            } catch (e) {
              throw e;
        }
    }


    public async pathBetweenFloors(pathDTO: IPathDTO): Promise<IPathResultDTO | Result<IPathResultDTO>> {
        let pathBetweenFloors: string[];
        let pathPerFloor: number[];

        if (pathDTO.origin == pathDTO.destination) {
            return Result.fail<IPathResultDTO>("Origin and destination can't be the same.");
        }


        const uri = config.connectionPlaneamento + "/pathBetweenFloors/" + pathDTO.origin + "/" + pathDTO.destination;
        let solution = new Promise<IPathResultDTO>((resolve) => {
            http.get(uri, (res) => {
                let data = '';
                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {
                    const pathResult: IPathResultDTO = {
                        pathBetweenFloors: JSON.parse(data).pathBetweenFloors,
                        pathPerFloor: JSON.parse(data).pathPerFloor
                    };
                    resolve(pathResult);
                });
            }).on("error", (err) => {
                console.log("Error: " + err.message);
            }).end();
              });

              await solution;

              return solution;
    }
}
