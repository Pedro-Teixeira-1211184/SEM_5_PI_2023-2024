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
import { forEach } from 'lodash';

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


    public async  turnToPlant(mapDTO: IMapDTO): Promise<IPlantDTO> {
        try {
        const plantDTO: IPlantDTO = {
            floorCode: `${mapDTO.buildingCode}${mapDTO.floorNumber}`,
            width: mapDTO.size.width,
            length: mapDTO.size.length,
            map: mapDTO.map.map((row) => [...row]),
        };

        //elevators = 2
        forEach(mapDTO.elevator, (elevator) => {
            plantDTO.map[elevator.localization.coordinates.x][elevator.localization.coordinates.y] = 2;
        }
        );

        //doors = 3
        forEach(mapDTO.rooms, (room) => {
            plantDTO.map[room.door.coordinates.x][room.door.coordinates.y] = 3;
        }
        );

        //passageways = 4
        forEach(mapDTO.passageways, (passageway) => {
            plantDTO.map[passageway.localization.coordinates.x][passageway.localization.coordinates.y] = 4;
        }
        );

        //walls = 1
        forEach(mapDTO.rooms, (room) => {
            plantDTO.map[room.dimensions.top.x][room.dimensions.top.y] = 1;
            plantDTO.map[room.dimensions.bottom.x][room.dimensions.bottom.y] = 1;
            for (let y=0, x=room.dimensions.top.x; y<room.dimensions.bottom.y; y++) {
                plantDTO.map[x][y] = 1;
            }
            for(let x=room.dimensions.top.x, y=room.dimensions.top.y; x<room.dimensions.bottom.x; x++) {
                plantDTO.map[x][y] = 1;
            }
            for(let y=room.dimensions.bottom.y, x=room.dimensions.bottom.x; y>room.dimensions.top.y; y--) {
                plantDTO.map[x][y] = 1;
            }
        }
        );
        return plantDTO;
    } catch (e) {
        throw e;

    }
}

  public async loadMap(buildingCode: string, floorNumber: number): Promise<Result<IPlantDTO>> {
    try {
      const mapOrError = await this.mapRepo.findByBuildingCodeAndFloorNumber(buildingCode, floorNumber);
      const mapExists = await this.mapRepo.exists(MapMapper.toDomain(mapOrError));
      if (!mapExists) {
        return Result.fail<IPlantDTO>("Map does not exist.");
      }
      const plantDTO = await this.turnToPlant(mapOrError);
      return Result.ok<IPlantDTO>(plantDTO);
    } catch (e) {
      throw e;
    }
  }

  public async listMaps(): Promise<Result<IPlantDTO[]>> {
    try {
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
    } catch (e) {
      throw e;
    }
  }

  public async pathBetweenFloors(pathDTO: IPathDTO): Promise<IPathResultDTO | Result<IPathResultDTO>> {
    /*let pathBetweenFloors: string[];
    let pathPerFloor: number[];*/

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

  public async getAll(): Promise<Result<IMapDTO[]>> {
    try {
      const mapOrError = await this.mapRepo.findAll();
      if (mapOrError.length == 0) {
        return Result.fail<IMapDTO[]>("No maps found.");
      }
      return Result.ok<IMapDTO[]>(mapOrError.map((map) => MapMapper.toDTO(map)));
    } catch (e) {
      throw e;
    }
  }
}
