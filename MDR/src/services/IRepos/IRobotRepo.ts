import { Repo } from "../../core/infra/Repo";
import {Robot} from "../../domain/robot";
import IRobotDTO from "../../dto/IRobotDTO";


export default interface IRobotRepo extends Repo<Robot> {
  save(robot: Robot): Promise<Robot>;
  findByDomainId(robotId: string): Promise<Robot>;
  findByCode(robotCode: string): Promise<Robot>;
  delete(robot: Robot): Promise<void>;
  exists(robot: Robot): Promise<boolean>;
  update(robot: Robot, id: string): Promise<Robot>;
  getAll(): Promise<IRobotDTO[]>;
}
