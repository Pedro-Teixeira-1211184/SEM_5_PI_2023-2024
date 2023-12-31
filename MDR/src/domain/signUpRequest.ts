import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Result} from "../core/logic/Result";
import {IUserDTO} from "../dto/IUserDTO";
import {SignUpRequestId} from "./signUpRequestId";
import {ISignUpRequestDTO} from "../dto/ISignUpRequestDTO";


interface UserProps {
  firstName: string;
  lastName: string;
  nif: string;
  email: string;
  password: string;
}

export class SignUpRequest extends AggregateRoot<UserProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get signUpRequestId(): SignUpRequestId {
    return SignUpRequestId.caller(this.id)
  }

  get email(): string {
    return this.props.email;
  }

  get firstName(): string {
    return this.props.firstName
  }

  get lastName(): string {
    return this.props.lastName;
  }

  get nif(): string {
    return this.props.nif;
  }

  get password(): string {
    return this.props.password;
  }

  set password(value: string) {
    this.props.password = value;
  }

  private constructor(props: UserProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: ISignUpRequestDTO, id?: UniqueEntityID): Result<SignUpRequest> {
    const firstName = props.firstName;
    const lastName = props.lastName;
    const nif = props.nif;
    const email = props.email;
    const password = props.password;

    const user = new SignUpRequest({
      firstName: firstName,
      lastName: lastName,
      nif: nif,
      email: email,
      password: password,
    }, id);
    return Result.ok<SignUpRequest>(user);
  }

}
