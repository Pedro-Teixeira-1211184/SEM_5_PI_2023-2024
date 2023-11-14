import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Result} from "../core/logic/Result";
import {UserId} from "./userId";
import {UserEmail} from "./userEmail";
import {Role} from "../domain/role";
import {UserPassword} from "./userPassword";
import {Guard} from "../core/logic/Guard";
import {IUserDTO} from "../dto/IUserDTO";


interface UserProps {
  firstName: string;
  lastName: string;
  email: string;
  password: string;
  role: string;
}

export class User extends AggregateRoot<UserProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get userId(): UserId {
    return UserId.caller(this.id)
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

  get password(): string {
    return this.props.password;
  }

  set password(value: string) {
    this.props.password = value;
  }

  get role(): string {
    return this.props.role;
  }

  set role(value: string) {
    this.props.role = value;
  }

  private constructor(props: UserProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: IUserDTO, id?: UniqueEntityID): Result<User> {
    const firstName = props.firstName;
    const lastName = props.lastName;
    const email = props.email;
    const password = props.password;
    const role = props.role;

    const user = new User({
      firstName: firstName,
      lastName: lastName,
      email: email,
      password: password,
      role: role
    }, id);
    return Result.ok<User>(user);
  }

}
