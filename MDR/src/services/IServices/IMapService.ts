import { Result } from "../../core/logic/Result";
import IMapDTO from "../../dto/IMapDTO";
import IPlantDTO from "../../dto/IPlantDTO";
import IPathResultDTO from "../../dto/IPathResultDTO";
import IPathDTO from "../../dto/IPathDTO";

export default interface IMapService {
  createMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>>;
  /*loadMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>>;
  deleteMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>>;
  listMaps(): Promise<Result<IPlantDTO[]>>;
  pathBetweenFloors(pathDTO: IPathDTO): Promise<Result<IPathResultDTO>>;
*/
}
