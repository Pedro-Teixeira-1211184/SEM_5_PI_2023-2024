import { Repo } from "../../core/infra/Repo";
import { Passageway } from "../../domain/passageway";
import IPassagewayDTO from "../../dto/IPassagewayDTO";

export default interface IPassagewayRepo extends Repo<Passageway> {
  save(passageway: Passageway): Promise<Passageway>;
  delete(passageway: Passageway): Promise<void>;
  exists(passageway: Passageway): Promise<boolean>;
  findFloorsInPassageways(floorId: string): Promise<boolean>;
  /*getPassagewaysInFloors(floorId: string): Promise<IPassagewayDTO>;*/
  findByDomainId(floorId: string): Promise<Passageway>;
}