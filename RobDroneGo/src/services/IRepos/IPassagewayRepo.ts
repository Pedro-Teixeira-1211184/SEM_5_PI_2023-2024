import { Repo } from "../../core/infra/Repo";
import { Passageway } from "../../domain/passageway";

export default interface IPassagewayRepo extends Repo<Passageway> {
  save(passageway: Passageway): Promise<Passageway>;
  delete(passageway: Passageway): Promise<void>;
  exists(passageway: Passageway): Promise<boolean>;
}