import { Repo } from "../../core/infra/Repo";
import {RobotType} from "../../domain/robotType";


export default interface IRobotTypeRepo extends Repo<RobotType> {
    save(robotType: RobotType): Promise<RobotType>;
    findByDomainId(robotTypeId: string): Promise<RobotType>;
    findByDesignation(designation: string): Promise<RobotType>;
    delete(robotType: RobotType): Promise<void>;
    exists(robotType: RobotType): Promise<boolean>;
}
