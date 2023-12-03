import {Inject, Service} from "typedi";
import config from "../../config";
import IUserController from "./IControllers/IUserController";
import IUserService from "../services/IServices/IUserService";
import e, {NextFunction, Request, Response} from "express";
import {IUserDTO} from "../dto/IUserDTO";
import {Result} from "../core/logic/Result";
import {ISignUpRequestDTO} from "../dto/ISignUpRequestDTO";

@Service()
export default class UserController implements IUserController /* TODO: extends ../core/infra/BaseController */ {

  jwt = require('jsonwebtoken');

  constructor(
    @Inject(config.services.user.name) private userServiceInstance: IUserService
  ) {
  }

  public async getAllUserRequests(req: Request, res: Response, next: NextFunction) {
    try {
      const userRequestsOrError = await this.userServiceInstance.getAllUserRequests() as Result<ISignUpRequestDTO[]>;
      if (userRequestsOrError.isFailure) {
        return res.status(403).json(userRequestsOrError.errorValue());
      }
      return res.status(200).json(userRequestsOrError.getValue());
    } catch (e) {
      return next(e);
    }
  }

  public async signUpRequest(req: Request, res: Response, next: NextFunction) {
    try {
      const userOrError = await this.userServiceInstance.SignUpRequest(req.body as ISignUpRequestDTO) as Result<ISignUpRequestDTO>;
      if (userOrError.isFailure) {
        console.log(userOrError.errorValue());
        return res.status(403).json(userOrError.errorValue());
      }

      const userDTO = userOrError.getValue();
      return res.status(201).json(userDTO);
    } catch (e) {
      return next(e);
    }
  }

  public async signUp(req: Request, res: Response, next: NextFunction) {
    try {
      const userOrError = await this.userServiceInstance.SignUp(req.body) as Result<IUserDTO>;
      if (userOrError.isFailure) {
        console.log(userOrError.errorValue());
        return res.status(403).json(userOrError.errorValue());
      }

      const userDTO = userOrError.getValue();
      return res.status(201).json(userDTO);
    } catch (e) {
      return next(e);
    }
  }

  public async signIn(req: Request, res: Response, next: NextFunction) {
    try {
      const params = {
        email: req.body.email,
        password: req.body.password
      };
      const userOrError = await this.userServiceInstance.SignIn(params.email, params.password) as Result<IUserDTO>;
      if (userOrError.isFailure) {
        console.log(userOrError.errorValue());
        return res.status(403).json(userOrError.errorValue());
      }
      let payload = {subject: userOrError.getValue().domainId};
      let token = this.jwt.sign(payload, 'secretKey')
      return res.status(200).json({userDTO: userOrError.getValue(), token});
    } catch (e) {
      return next(e);
    }
  }

  public async isSignedIn(req: Request, res: Response, next: NextFunction) {
    try {
      const userOrError = await this.userServiceInstance.IsSignedIn();
      if (userOrError == null) {
        return res.status(403).json({message: "User is not signed in!"});
      } else {
        return res.status(200).json(userOrError);
      }
    } catch (e) {
      return next(e);
    }
  }

  public async signOut(req: Request, res: Response, next: NextFunction) {
    try {
      await this.userServiceInstance.Logout();
      return res.status(200).json({message: "User signed out!"});
    } catch (e) {
      return next(e);
    }
  }

  public async deleteUser(req: Request, res: Response, next: NextFunction) {
    try {
      const email = req.params.email;
      const sucOrError = await this.userServiceInstance.deleteUser(email);
      if (sucOrError.isFailure) {
        return res.status(403).json(sucOrError.errorValue());
      }
      return res.status(200).json({message: "User deleted!"});
    } catch (e) {
      return next(e);
    }
  }

  public async getUser(req: Request, res: Response, next: NextFunction) {
    try {
      const email = req.params.email;
      const userOrError = await this.userServiceInstance.getUserByEmail(email) as Result<IUserDTO>;
      if (userOrError.isFailure) {
        return res.status(403).json(userOrError.errorValue());
      }
      return res.status(200).json(userOrError.getValue());
    } catch (e) {
      return next(e);
    }
  }

  public async deleteUserRequest(req: Request, res: Response, next: NextFunction) {
    try {
      const email = req.params.email;
      const sucOrError = await this.userServiceInstance.deleteUserRequest(email);
      if (sucOrError.isFailure) {
        return res.status(403).json(sucOrError.errorValue());
      }
      return res.status(200).json({message: "User request deleted!"});
    } catch (e) {
      return next(e);
    }
  }

}
