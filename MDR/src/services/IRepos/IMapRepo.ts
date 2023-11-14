import { Repo } from "../../core/infra/Repo";
import {Map} from "../../domain/map";


export default interface IMapRepo extends Repo<Map> {
  save(map: Map): Promise<Map>;
  exists(map: Map): Promise<boolean>;
}
