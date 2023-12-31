using Microsoft.EntityFrameworkCore.Migrations;

namespace MDU.Migrations
{
    public partial class RemoveUserIdOfTask : Migration
    {
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "UserID",
                table: "Tasks");
        }

        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.AddColumn<string>(
                name: "UserID",
                table: "Tasks",
                type: "text",
                nullable: true);
        }
    }
}
