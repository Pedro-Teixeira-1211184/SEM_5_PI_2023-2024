import { Repo } from "../../core/infra/Repo";
import { Robot } from "../../domain/building";
import {IBuildingPersistence} from "../../dataschema/IBuildingPersistence";
import IBuildingDTO from "../../dto/IBuildingDTO";


export default interface IBuildingRepo extends Repo<Robot> {
  save(building: Robot): Promise<Robot>;
  findByDomainId(buildingId: string): Promise<Robot>;
  findByCode(buildingCode: string): Promise<Robot>;
  delete(building: Robot): Promise<void>;
  exists(building: Robot): Promise<boolean>;
  update(buildingCode: string, updatedFields: Partial<IBuildingDTO>): Promise<Robot | null>;
  getAll(): Promise<IBuildingDTO[]>;
}
