import { Repo } from "../../core/infra/Repo";
import { Floor } from "../../domain/floor";


export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;
  findByDomainId(floorId: string): Promise<Floor>;
  delete(floor: Floor): Promise<void>;
  exists(floor: Floor): Promise<boolean>;
}
