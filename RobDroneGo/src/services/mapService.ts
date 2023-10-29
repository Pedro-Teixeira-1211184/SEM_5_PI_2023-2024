import {Service, Inject} from 'typedi';
import config from "../../config";
import IMapService from "./IServices/IMapService";
import {Result} from "../core/logic/Result";
import IMapDTO from "../dto/IMapDTO";
import IMapRepo from './IRepos/IMapRepo';
import {Map} from "../domain/map";
import {MapMapper} from "../mappers/MapMapper";


@Service()

export default class MapService implements IMapService {

  constructor(
    @Inject(config.repos.map.name) private mapRepo: IMapRepo
  ) {
  }

  public async createMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
    try {

      //validate map info
      /*
      const isValid = await this.validateFloorInfo(mapDTO);
      if(isValid.isFailure){
        return Result.fail<IMapDTO>(isValid.errorValue());
      }

       */

      const mapOrError = await Map.create(mapDTO);
      if (mapOrError.isFailure) {
        return Result.fail<IMapDTO>(mapOrError.errorValue());
      }

      const mapResult = mapOrError.getValue();

      //save floor
      const mapCreated = await this.mapRepo.save(mapResult);

      if (mapCreated === null) {
        return Result.fail<IMapDTO>('Floor already exists');
      }

      const floorDTOResult = MapMapper.toDTO(mapResult) as IMapDTO;
      return Result.ok<IMapDTO>(floorDTOResult)
    } catch (e) {
      throw e;
    }
  }
/*
  public async validateFloorInfo(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
    try{

    }catch (e) {
      throw e;
    }
  }

 */


}
