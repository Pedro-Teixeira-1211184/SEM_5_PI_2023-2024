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

  /*
    public async validateFloorInfo(mapDTO: IMapDTO): Promise<Result<IMapDTO>> {
      try{

      }catch (e) {
        throw e;
      }
    }

   */


}
