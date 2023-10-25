import {Result} from "../../core/logic/Result";
import IRobotDTO from "../../dto/IRobotDTO";
import IRobotTypeDTO from "../../dto/IRobotTypeDTO";

export default interface IRobotService {
  createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;

  createRobotType(robotTypeDTO: IRobotTypeDTO): Promise<Result<IRobotTypeDTO>>;

  updateRobot(robot_id: string, body: IRobotDTO): Promise<Result<IRobotDTO>>;
}
