import { Repo } from "../../core/infra/Repo";
import { Building } from "../../domain/building";
import {IBuildingPersistence} from "../../dataschema/IBuildingPersistence";
import IBuildingDTO from "../../dto/IBuildingDTO";


export default interface IBuildingRepo extends Repo<Building> {
  save(building: Building): Promise<Building>;
  findByDomainId(buildingId: string): Promise<Building>;
  delete(building: Building): Promise<void>;
  exists(building: Building): Promise<boolean>;
  update(buildingCode: string, updatedFields: Partial<IBuildingDTO>): Promise<Building | null>;
  getAll(): Promise<IBuildingDTO[]>;
}
