import {Service, Inject} from 'typedi';
import config from "../../config";
import IFloorRepo from "../services/IRepos/IFloorRepo";
import IFloorService from "../services/IServices/IFloorService";
import IFloorDTO from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import {FloorMapper} from "../mappers/FloorMapper";
import {Floor} from "../domain/floor";
import IBuildingRepo from "../services/IRepos/IBuildingRepo";

@Service()

export default class FloorService implements IFloorService {

  constructor(
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo
  ) {
  }

  public async createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {
      const floorOrError = await Floor.create(floorDTO);
      if (floorOrError.isFailure) {
        return Result.fail<IFloorDTO>(floorOrError.errorValue());
      }

      const floorResult = floorOrError.getValue();

      //save floor
      const floorCreated = await this.floorRepo.save(floorResult);

      if (floorCreated === null) {
        return Result.fail<IFloorDTO>('Floor already exists');
      }

      const floorDTOResult = FloorMapper.toDTO(floorResult) as IFloorDTO;
      return Result.ok<IFloorDTO>(floorDTOResult)
    } catch (e) {
      throw e;
    }
  }

  public async findFloorsByBuildingCode(buildingCode: string): Promise<Result<IFloorDTO[]>> {
    try {
      const buildingOrError = await this.buildingRepo.findByCode(buildingCode);
      if (buildingOrError == null) {
        return Result.fail<IFloorDTO[]>('Building not found');
      }
      const floors = await this.floorRepo.findFloorsByBuildingCode(buildingCode);
      if (floors.length == 0) {
        return Result.fail<IFloorDTO[]>('No floors found');
      }

      return Result.ok<IFloorDTO[]>(floors)
    } catch (e) {
      console.log('Error in floorService.findFloorsByBuildingCode', e);
      throw e;
    }
  }

  public async updateFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {
      const updatedFloor = await this.floorRepo.update(floorDTO.buildingCode, floorDTO.number, floorDTO);
      if (updatedFloor == null) {
        console.log('Floor not found');
        return Result.fail<IFloorDTO>('Floor not found');
      }

      const updatedFloorDTO = FloorMapper.toDTO(updatedFloor);

      return Result.ok<IFloorDTO>(updatedFloorDTO);
    } catch (e) {
      throw e;
    }
  }

  public async findFloorByCode(floorCode: string): Promise<Result<IFloorDTO>> {
    try {
      const floor = await this.floorRepo.findFloorByCode(floorCode);
      if (floor == null) {
        return Result.fail<IFloorDTO>('Floor not found');
      }

      const floorDTO = FloorMapper.toDTO(floor);

      return Result.ok<IFloorDTO>(floorDTO);
    } catch (e) {
      throw e;
    }
  }

}
