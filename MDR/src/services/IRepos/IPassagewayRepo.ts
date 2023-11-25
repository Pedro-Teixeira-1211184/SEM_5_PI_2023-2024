import {Repo} from "../../core/infra/Repo";
import {Passageway} from "../../domain/passageway";
import IFloorDTO from "../../dto/IFloorDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";

export default interface IPassagewayRepo extends Repo<Passageway> {
  save(passageway: Passageway): Promise<Passageway>;

  delete(passageway: Passageway): Promise<void>;

  exists(passageway: Passageway): Promise<boolean>;

  findFloorsInPassageways(floorId: string): Promise<boolean>;

  findByDomainId(floorId: string): Promise<Passageway>;

  findByFloorCodes(floorCode1: string, floorCode2: string): Promise<Passageway>;

  getPassagewaysInBuildings(floors1: Array<IFloorDTO>, floors2: Array<IFloorDTO>): Promise<Array<IPassagewayDTO>>;

  update(floorID1: string, floorID2: string, updatedFields: Partial<IPassagewayDTO>): Promise<Passageway | null>;

  getPassagewaysInFloor(floorCode: string): Promise<Array<IPassagewayDTO>>;

  getAll(): Promise<Array<IPassagewayDTO>>;
}
