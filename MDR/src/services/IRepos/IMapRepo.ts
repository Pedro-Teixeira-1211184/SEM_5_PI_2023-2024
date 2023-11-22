import { Repo } from "../../core/infra/Repo";
import {Map} from "../../domain/map";
import IMapDTO from "../../dto/IMapDTO";

export default interface IMapRepo extends Repo<Map> {
  save(map: Map): Promise<Map>;
  exists(map: Map): Promise<boolean>;
  findByBuildingCodeAndFloorNumber(buildingCode: string, floorNumber: number): Promise<IMapDTO>;
  findAll(): Promise<Map[]>;
}
