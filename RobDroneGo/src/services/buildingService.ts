import {Service, Inject} from 'typedi';
import config from "../../config";
import IBuildingDTO from '../dto/IBuildingDTO';
import {Building} from "../domain/building";
import IBuildingRepo from "../services/IRepos/IBuildingRepo";
import IBuildingService from "./IServices/IBuildingService";
import {Result} from "../core/logic/Result";
import {BuildingMapper} from "../mappers/BuildingMapper";


@Service()

export default class BuildingService implements IBuildingService {

  constructor(
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo
  ) {
  }

  public async createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {

      const buildingOrError = await Building.create(buildingDTO);
      if (buildingOrError.isFailure) {
        return Result.fail<IBuildingDTO>(buildingOrError.errorValue());
      }

      const buildingResult = buildingOrError.getValue();

      //save building
      const buildingCreated = await this.buildingRepo.save(buildingResult);

      if (buildingCreated === null) {
        return Result.fail<IBuildingDTO>('Building not created');
      }

      const buildingDTOResult = BuildingMapper.toDTO(buildingResult) as IBuildingDTO;
      return Result.ok<IBuildingDTO>(buildingDTOResult)
    } catch (e) {
      throw e;
    }

  }

  //updateBuilding that uses the buildingRepo to update the building
  public async updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {
      //Update the Building in the Repository
      const updatedBuilding = await this.buildingRepo.update(buildingDTO.code, buildingDTO);

      if (updatedBuilding == null) {
        console.log('Building not found');
        return Result.fail<IBuildingDTO>('Building not found');
      }

      //Convert the updated building to DTO and return it in the result
      const updatedBuildingDTO = BuildingMapper.toDTO(updatedBuilding);

      return Result.ok<IBuildingDTO>(updatedBuildingDTO);

    } catch (error) {
      throw error;
    }
  }

  public async getAll(): Promise<Result<IBuildingDTO[]>> {
    const buildings = await this.buildingRepo.getAll();
    if (buildings.length == 0) {
      return Result.fail<IBuildingDTO[]>("No buildings found");
    }
    return Result.ok<IBuildingDTO[]>(buildings);
  }

  public async getBuildingByCode(buildingCode: string): Promise<Result<IBuildingDTO>> {
    try {
      //Get the building from the Repository
      const building = await this.buildingRepo.findByCode(buildingCode);

      if (building == null) {
        console.log('Building not found');
        return Result.fail<IBuildingDTO>('Building not found');
      }

      //Convert the building to DTO and return it in the result
      const buildingDTO = BuildingMapper.toDTO(building);

      return Result.ok<IBuildingDTO>(buildingDTO);

    } catch (error) {
      throw error;
    }
  }


  public async findByMinMaxFloors(minFloors: number, maxFloors: number): Promise<Result<IBuildingDTO[]>> {
    try{
      const buildings = await this.buildingRepo.findByMinMaxFloors(minFloors, maxFloors);
    
    if (buildings.length == 0) {
      return Result.fail<IBuildingDTO[]>("No buildings found");
    }
    return Result.ok<IBuildingDTO[]>(buildings);
  

  } catch (error) {
    console.log('Error in BuildingService.findByMinMaxFloors(): ', error);
    throw error;
  }
  }


}
