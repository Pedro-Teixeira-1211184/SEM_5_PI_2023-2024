import { Repo } from "../../core/infra/Repo";
import { Floor } from "../../domain/floor";
import IFloorDTO from "../../dto/IFloorDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";


export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;
  findByDomainId(floorId: string): Promise<Floor>;
  delete(floor: Floor): Promise<void>;
  exists(floor: Floor): Promise<boolean>;
  findFloorsByBuildingCode(buildingCode: string): Promise<IFloorDTO[]>;
  /*findFloorsByPassageways(floorArray: IFloorDTO[]): Promise<IFloorDTO[]>;*/
  /*findWherePassagewaysLeads(passageway: IFloorDTO[]): Promise<string[]>;*/
  update(buildingCode: string, floorNumber: number, updatedFields: Partial<IFloorDTO>): Promise<Floor | null>;
}
