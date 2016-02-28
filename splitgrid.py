# Blender script for splitting geometry by grid
# don't forget to triangulate while exporting
import bpy, bmesh
from bpy import context as C

bpy.ops.object.mode_set(mode='EDIT')

bm = bmesh.from_edit_mesh(C.object.data)

edges = []

step = 0.5
minn = -20
maxn = 20

for i in range(minn, maxn, 1):
        ret = bmesh.ops.bisect_plane(bm, geom=bm.verts[:]+bm.edges[:]+bm.faces[:], plane_co=(i*step,0,0), plane_no=(-1,0,0))
        bmesh.ops.split_edges(bm, edges=[e for e in ret['geom_cut'] if isinstance(e, bmesh.types.BMEdge)])

for i in range(minn, maxn, 1):
        ret = bmesh.ops.bisect_plane(bm, geom=bm.verts[:]+bm.edges[:]+bm.faces[:], plane_co=(0,i*step,0), plane_no=(0,1,0))
        bmesh.ops.split_edges(bm, edges=[e for e in ret['geom_cut'] if isinstance(e, bmesh.types.BMEdge)])

for i in range(minn, maxn, 1):
        ret = bmesh.ops.bisect_plane(bm, geom=bm.verts[:]+bm.edges[:]+bm.faces[:], plane_co=(0,0,i*step), plane_no=(0,0,-1))
        bmesh.ops.split_edges(bm, edges=[e for e in ret['geom_cut'] if isinstance(e, bmesh.types.BMEdge)])

bmesh.update_edit_mesh(C.object.data)

bpy.ops.mesh.separate(type='LOOSE')
bpy.ops.object.mode_set(mode='OBJECT')
