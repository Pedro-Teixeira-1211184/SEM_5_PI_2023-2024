import { Repo } from "../../core/infra/Repo";
import { Floor } from "../../domain/floor";
import IFloorDTO from "../../dto/IFloorDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";


export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;
  findByDomainId(floorId: string): Promise<Floor>;
  delete(floor: Floor): Promise<void>;
  exists(floor: Floor): Promise<boolean>;
  existsByBCodeAndNumber(buildingCode: string, floorNumber: number): Promise<boolean>;
  findFloorsByBuildingCode(buildingCode: string): Promise<IFloorDTO[]>;
  findByBCodeAndNumber(buildingCode: string,  number: number): Promise<Floor>;
  /*findFloorsByPassageways(floorArray: IFloorDTO[]): Promise<IFloorDTO[]>;*/
  /*findWherePassagewaysLeads(passageway: IFloorDTO[]): Promise<string[]>;*/
  update(buildingCode: string, number: number, updatedFields: Partial<IFloorDTO>): Promise<Floor | null>;
  findFloorByCode(floorCode: string): Promise<Floor>;
  getAll(): Promise<IFloorDTO[]>;
}
