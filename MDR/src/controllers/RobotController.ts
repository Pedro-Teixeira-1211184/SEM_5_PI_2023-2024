import {Inject, Service} from 'typedi';
import config from "../../config";
import {StatusCodes} from "http-status-codes";
import IRobotController from "./IControllers/IRobotController";
import IRobotService from "../services/IServices/IRobotService";
import e, {NextFunction, Request, Response} from "express";
import IRobotDTO from "../dto/IRobotDTO";
import {Result} from "../core/logic/Result";
import IRobotTypeDTO from "../dto/IRobotTypeDTO";


@Service()
export default class RobotController implements IRobotController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.robot.name) private robotServiceInstance: IRobotService
  ) {
  }

  public async createRobot(req: Request, res: Response, next: NextFunction) {
    try {
      const robotOrError = await this.robotServiceInstance.createRobot(req.body as IRobotDTO) as Result<IRobotDTO>;
      if (robotOrError.isFailure) {
        console.log(robotOrError.errorValue());
        return res.status(StatusCodes.BAD_REQUEST).json("Robot not created");
      }

      const robotDTO = robotOrError.getValue();
      return res.status(StatusCodes.CREATED).json("Robot created");
    } catch (e) {
      return next(e);
    }
  }

  public async createRobotType(req: Request, res: Response, next: NextFunction) {
    try {
      const robotTypeOrError = await this.robotServiceInstance.createRobotType(req.body as IRobotTypeDTO) as Result<IRobotTypeDTO>;
      if (robotTypeOrError.isFailure) {
        console.log(robotTypeOrError.errorValue());
        return res.status(StatusCodes.BAD_REQUEST).json(robotTypeOrError.errorValue());
      }

      const robotTypeDTO = robotTypeOrError.getValue();
      return res.status(StatusCodes.CREATED).json(robotTypeDTO);
    } catch (e) {
      return next(e);
    }
  }

  public async updateRobot(req: Request, res: Response, next: NextFunction) {
    try {
      const robotOrError = await this.robotServiceInstance.updateRobot(req.params.robot_id, req.body as IRobotDTO) as Result<IRobotDTO>;
      if (robotOrError.isFailure) {
        console.log(robotOrError.errorValue());
        return res.status(StatusCodes.BAD_REQUEST).json(robotOrError.errorValue());
      }

      const robotDTO = robotOrError.getValue();
      return res.status(StatusCodes.OK).json(robotDTO);
    } catch (e) {
      return next(e);
    }
  }

  public async getAllRobots(req: Request, res: Response, next: NextFunction) {
    try {
      const robots = await this.robotServiceInstance.getAll();

      if (robots.isFailure) {
        return res.status(StatusCodes.BAD_REQUEST).json(robots.errorValue());
      }

      const robotsDTO = robots.getValue();

      return res.status(StatusCodes.OK).json(robotsDTO);

    } catch (e) {
      return next(e);
    }
  }

  public async getRobotType(req: Request, res: Response, next: NextFunction) {
    try {
      const types = await this.robotServiceInstance.getAllTypes() as Result<IRobotTypeDTO[]>

      if (types.isFailure) {
        return res.status(StatusCodes.BAD_REQUEST).json(types.errorValue());
      }

      const typesDTO = types.getValue();

      return res.status(StatusCodes.OK).json(typesDTO);

    } catch (e) {
      return next(e);
    }
  }
}
