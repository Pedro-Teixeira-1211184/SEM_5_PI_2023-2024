import { Service, Inject } from 'typedi';
import config from "../../config";
import IBuildingDTO from '../dto/IBuildingDTO';
import { Building } from "../domain/building";
import IBuildingRepo from "../services/IRepos/IBuildingRepo";
import IBuildingService from "./IServices/IBuildingService";
import { Result } from "../core/logic/Result";
import { BuildingMapper } from "../mappers/BuildingMapper";


@Service()

export default class BuildingService implements IBuildingService {

  constructor(
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo
  ) { }

  public async createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {

      const buildingOrError = await Building.create(buildingDTO);
      if (buildingOrError.isFailure) {
        return Result.fail<IBuildingDTO>(buildingOrError.errorValue());
      }

      const buildingResult = buildingOrError.getValue();

      await this.buildingRepo.save(buildingResult);

      const buildingDTOResult = BuildingMapper.toDTO(buildingResult) as IBuildingDTO;
      return Result.ok<IBuildingDTO>(buildingDTOResult)
    } catch (e) {
      throw e;
    }

  }

  //updateBuilding that uses the buildingRepo to update the building
  // @ts-ignore
  public async updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    // not implemented
  }
}
