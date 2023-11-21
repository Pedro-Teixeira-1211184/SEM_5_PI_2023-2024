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
import { http } from 'winston';
import IPathDTO from '../dto/IPathDTO';
import IPathResultDTO from '../dto/IPathResultDTO';


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

    /*public async loadMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
        try {
            const mapOrError = await this.mapRepo.findByBuildingCodeAndFloorNumber(mapDTO.buildingCode, mapDTO.floorNumber) as Result<Map>;
            if (mapOrError.isFailure) {
                return Result.fail<IMapDTO>(mapOrError.errorValue());
            }
            const map = mapOrError.getValue();
            const mapExists = await this.mapRepo.exists(map);
            if (!mapExists) {
                return Result.fail<IMapDTO>("Map does not exist.");
            }
            const mapDocument = await this.mapRepo.save(map);
            mapDTO = MapMapper.toDTO(mapDocument);

            return Result.ok<IMapDTO>(mapDTO);
        } catch (e) {
            throw e;
        }
    }*/

    /*public async deleteMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
        try {
            const mapOrError = await this.mapRepo.findByBuildingCodeAndFloorNumber(mapDTO.buildingCode, mapDTO.floorNumber) as Result<Map>;
            if (mapOrError.isFailure) {
                return Result.fail<IMapDTO>(mapOrError.errorValue());
            }
            const map = mapOrError.getValue();
            const mapExists = await this.mapRepo.exists(map);
            if (!mapExists) {
                return Result.fail<IMapDTO>("Map does not exist.");
            }
            const mapDocument = await this.mapRepo.delete(map);
            mapDTO = MapMapper.toDTO(mapDocument);

            return Result.ok<IMapDTO>(mapDTO);
        } catch (e) {
            throw e;
        }
    }*/

    /*public async listMaps(): Promise<Result<IPlantDTO[]>> {
        try{
            const mapOrError = await this.mapRepo.findAll();
            if (mapOrError.isFailure) {
                return Result.fail<IPlantDTO[]>(mapOrError.errorValue());
            }
            let plants: IPlantDTO[] = [];
            for (let i=0; i<mapOrError.length; i++) {
                
                let plantDTO = mapOrError[i].mapa as IPlantDTO;

                const plant={
                    floorCode = mapOrError[i].floorCode,
                    width = plantDTO.width,
                    length = plantDTO.length,
                    map = plantDTO.map,
                    passageways = plantDTO.passageways,
                    elevator = plantDTO.elevator,
                    rooms = plantDTO.rooms
                }as IPlantDTO;
                plants.push(plant);
            }
            return Result.ok<IPlantDTO[]>(plants);
        }catch(e){
            throw e;
        }
    }*/

    /*public async pathBetweenFloors(pathDTO: IPathDTO): Promise<Result<IPathResultDTO>> {
        let pathBetweenFloors: string[];
        let pathPerFloor: number[];

        const uri = config.connectionPlaneamento + "/pathBetweenFloors/" + pathDTO.origin + "/" + pathDTO.destination;

        return await fetch(uri, {
            method : 'GET',
            headers: {
                'Content-Type': 'application/json'
            }
        }).then((response) => {
            return response.json();
        }).then((data) => {
            pathBetweenFloors = data.path;
            pathPerFloor = data.pathPerFloor;
            return Result.ok<IPathResultDTO>({pathBetweenFloors, pathPerFloor});
        }).catch((error) => {
            return Result.fail<IPathResultDTO>(error);
        }
        );

        return pathResult.create({
            pathBetweenFloors: pathBetweenFloors,
            pathPerFloor: pathPerFloor
        } as IPathResultDTO);
    }*/
}
