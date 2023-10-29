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
import {Floor} from "../domain/floor";


@Service()

export default class MapService implements IMapService {

  constructor(
    @Inject(config.repos.map.name) private mapRepo: IMapRepo,
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
    @Inject(config.repos.passageway.name) private passagewayRepo: IPassagewayRepo,
    @Inject(config.repos.elevator.name) private elevatorRepo: IElevatorRepo
  ) {
  }

  public async createMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
    try {
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
          const test = await this.floorRepo.findByDomainId(roomExists.floorID);
          if (test == null || test.buildingCode != mapDTO.buildingCode || test.number != mapDTO.floorNumber) {
            return Result.fail<IMapDTO>("Room " + room.name + " does not belong to this floor.");
          }
        }
      }
      /*

      const floor = await this.floorRepo.findByBCodeAndNumber(mapDTO.buildingCode, mapDTO.floorNumber)

      //verify if passageways exist
      for (let passageway of mapDTO.passageways) {
        //array of passageways of this floor
        const passagewayExists = await this.passagewayRepo.findPassagewayByFloorID1AndFloorID2(floor.id.toString());

        //TODO: check if array passagewayExists contains each one of mapDTO.passageways

      }

       */

      //verify if elevator exists
      const elevatorExists = await this.elevatorRepo.findByBuildingCode(mapDTO.buildingCode);
      //get floors where elevator exists
      const floorsWithElevator = elevatorExists.floorNumbers.split(",");
      //check if elevator exists in this floor
      if (!floorsWithElevator.includes(mapDTO.floorNumber.toString())) {
        return Result.fail<IMapDTO>("Elevator does not exist in this floor.");
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


}
