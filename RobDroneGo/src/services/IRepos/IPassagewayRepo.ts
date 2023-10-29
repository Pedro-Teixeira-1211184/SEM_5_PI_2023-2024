import { Repo } from "../../core/infra/Repo";
import { Passageway } from "../../domain/passageway";
import IFloorDTO from "../../dto/IFloorDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";

export default interface IPassagewayRepo extends Repo<Passageway> {
  save(passageway: Passageway): Promise<Passageway>;
  delete(passageway: Passageway): Promise<void>;
  exists(passageway: Passageway): Promise<boolean>;
  findFloorsInPassageways(floorId: string): Promise<boolean>;
  findPassagewayByFloorID1AndFloorID2(floorID1: string): Promise<Passageway[]>;
  /*getPassagewaysInFloors(floorId: string): Promise<IPassagewayDTO>;*/
  findByDomainId(floorId: string): Promise<Passageway>;
  getPassagewaysInBuildings(floors1: Array<IFloorDTO>, floors2: Array<IFloorDTO>): Promise<Array<IPassagewayDTO>>;
  update(floorID1: string, floorID2: string, updatedFields: Partial<IPassagewayDTO>): Promise<Passageway | null>;
}
