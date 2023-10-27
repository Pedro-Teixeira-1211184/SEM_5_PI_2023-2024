import { Repo } from "../../core/infra/Repo";
import { Elevator } from "../../domain/elevator";
import {IElevatorPersistence} from "../../dataschema/IElevatorPersistence";
import IElevatorDTO from "../../dto/IElevatorDTO";

export default interface IElevatorRepo extends Repo<Elevator> {
    save(elevator: Elevator): Promise<Elevator>;
    exists(elevator: Elevator): Promise<boolean>;
    delete(elevator: Elevator): Promise<void>;
    findByDomainId(elevatorId: string): Promise<Elevator>;
}