import {Inject, Service} from 'typedi';
import config from "../../config";
import {StatusCodes} from "http-status-codes";
import IRobotController from "./IControllers/IRobotController";
import IRobotService from "../services/IServices/IRobotService";
import {NextFunction, Request, Response} from "express";
import IRobotDTO from "../dto/IRobotDTO";
import {Result} from "../core/logic/Result";


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
                return res.status(StatusCodes.BAD_REQUEST).json(robotOrError.errorValue());
            }

            const robotDTO = robotOrError.getValue();
            return res.status(StatusCodes.CREATED).json(robotDTO);
        } catch (e) {
            return next(e);
        }
    }
}
