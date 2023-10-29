import { Result } from "../../core/logic/Result";
import IMapDTO from "../../dto/IMapDTO";

export default interface IMapService {
  createMap(mapDTO: IMapDTO): Promise<Result<IMapDTO>>;
}
