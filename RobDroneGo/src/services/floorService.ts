import {Service, Inject} from 'typedi';
import config from "../../config";
import IFloorRepo from "../services/IRepos/IFloorRepo";
import IFloorService from "../services/IServices/IFloorService";
import IFloorDTO from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import {FloorMapper} from "../mappers/FloorMapper";
import {Floor} from "../domain/floor";


@Service()

export default class FloorService implements IFloorService {

  constructor(
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo
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


}