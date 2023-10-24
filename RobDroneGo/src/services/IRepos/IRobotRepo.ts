import { Repo } from "../../core/infra/Repo";
import {Robot} from "../../domain/robot";


export default interface IRobotRepo extends Repo<Robot> {
  save(robot: Robot): Promise<Robot>;
  findByDomainId(robotId: string): Promise<Robot>;
  delete(robot: Robot): Promise<void>;
  exists(robot: Robot): Promise<boolean>;
}
